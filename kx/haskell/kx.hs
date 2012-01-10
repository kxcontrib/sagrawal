{-# LANGUAGE MagicHash, FlexibleInstances, BangPatterns #-}
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Foreign (Ptr, peek, poke, sizeOf,plusPtr)
import Foreign.C.Types (CChar)
import GHC.Int
import GHC.Word (Word8,Word16,Word32,Word64)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import Text.Printf (printf)
import Data.Bits (shiftR)
import Data.ByteString.Internal (unsafeCreate,ByteString)
import Data.ByteString (unpack)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import qualified Control.Monad as CM (forM_)
import Criterion.Main

 
data Elems = B {-# UNPACK #-} !Word8 -- Boolean
             | BV {-# UNPACK #-} !(SV.Vector Word8) -- Boolean list
             | X {-# UNPACK #-} !Word8 -- Byte
             | XV {-# UNPACK #-} !(SV.Vector Word8) -- Byte list
             | H {-# UNPACK #-} !Int16 -- Short
             | HV {-# UNPACK #-} !(SV.Vector Int16) -- Short list
             | I {-# UNPACK #-} !Int32 -- Int
             | IV {-# UNPACK #-} !(SV.Vector Int32) -- Int list
             | J {-# UNPACK #-} !Int64 -- Long
             | JV {-# UNPACK #-} !(SV.Vector Int64) -- Long list
             | E {-# UNPACK #-} !Float -- Real
             | EV {-# UNPACK #-} !(SV.Vector Float) -- Real list
             | F {-# UNPACK #-} !Double -- Float
             | FV {-# UNPACK #-} !(SV.Vector Double) -- Float list
             | C {-# UNPACK #-} !CChar -- Char
             | CV {-# UNPACK #-} !(SV.Vector CChar) -- Char list: just a list of chars - no need to store length or total bytes
             | S {-# UNPACK #-} !(SV.Vector CChar) -- Symbol - null terminated C string
             | SV {-#UNPACK #-} !Int {-# UNPACK #-} !(SV.Vector CChar) -- Symbol list: Int stores the number of null-terminated C Strings
             | T {-# UNPACK #-} !Int {-# UNPACK #-} !(V.Vector Elems) -- Q table: Int stores total bytes needed to encode table in ByteString
             | L {-# UNPACK #-} !(V.Vector Elems) -- General list of elements, for example, remote function call of the form ("insert";`t;table) 
                deriving (Show)

-- | Function to return total size in bytes taken to store the data from Elems - we will use it to calculate total bytes needed to build the ByteString for IPC to Q server
size :: Elems -> Int
size (B _) = 1 + sizeOf (undefined :: Word8)
size (BV x) = 6 + (sizeOf (undefined :: Word8)) * (SV.length x)
size (X _) = 1 + sizeOf (undefined :: Word8)
size (XV x) = 6 + (sizeOf (undefined :: Word8)) * (SV.length x)
size (H _) = 1 + sizeOf (undefined :: Int16)
size (HV x) = 6 + (sizeOf (undefined :: Int16)) * (SV.length x)
size (I _) = 1 + sizeOf (undefined :: Int32)
size (IV x) = 6 + (sizeOf (undefined :: Int32)) * (SV.length x)
size (J _) = 1 + sizeOf (undefined :: Int64)
size (JV x) = 6 + (sizeOf (undefined :: Int64)) * (SV.length x)
size (E _) = 1 + sizeOf (undefined :: Float)
size (EV x) = 6 + (sizeOf (undefined :: Float)) * (SV.length x)
size (F _) = 1 + sizeOf (undefined :: Double)
size (FV x) = 6 + (sizeOf (undefined :: Double)) * (SV.length x)
size (C _) = 1 + sizeOf (undefined :: CChar)
size (CV x) = 6 + (sizeOf (undefined :: CChar)) * (SV.length x)
size (S x) = 1 + (sizeOf (undefined :: CChar)) * (SV.length x)
size (SV _ x) = size (CV x)
size (T n _) = n
size (L x) = V.foldl' (\x y -> x + size y) 6 x
{-# INLINE size #-}

-- Function to return number of Q elements
numElements :: Elems -> Int
numElements (BV x) = SV.length x
numElements (XV x) = SV.length x
numElements (HV x) = SV.length x
numElements (IV x) = SV.length x
numElements (JV x) = SV.length x
numElements (EV x) = SV.length x
numElements (FV x) = SV.length x
numElements (CV x) = SV.length x
numElements (SV n _) = n
numElements (L x) = V.length x 
numElements _ = undefined

-- | Function to return q type of the object
qType :: Elems -> Int8
qType (B _) = -1
qType (BV _) = 1
qType (X _) = -4 
qType (XV _) = 4
qType (H _) = -5
qType (HV _) = 5
qType (I _) = -6
qType (IV _) = 6
qType (J _) = -7
qType (JV _) = 7
qType (E _) = -8
qType (EV _) = 8
qType (F _) = -9 
qType (FV _) = 9
qType (C _) = -10
qType (CV _) = 10
qType (S _) = -11
qType (SV _ _) = 11
qType (T _ _) = 98
qType (L _) = 0

-- | Function to write Q type in ByteString
putType :: Elems -> Ptr Word8 -> IO (Ptr Word8)
putType x ptr = poke ptr (fromIntegral $ qType x ::GHC.Word.Word8) >> return (ptr `plusPtr` 1)

-- | Function to write Q attribute in ByteString
putAttr :: Ptr Word8 -> IO (Ptr Word8)
putAttr ptr = poke ptr (0 :: GHC.Word.Word8) >> return (ptr `plusPtr` 1)

-- write type, attr, length for list elements 
putListHeader :: Elems -> Ptr Word8 -> IO (Ptr Word8)
putListHeader x ptr = putType x ptr >>= \p1 ->  putAttr p1 
                    >>= \p2 -> putWord32 (unsafeCoerce ((fromIntegral $ numElements x) :: GHC.Int.Int32)) p2
                    >> return (p2 `plusPtr` 4)


-- | Function to build Q IPC representation (except message header)
qBytes :: Elems -> Ptr Word8 -> IO (Ptr Word8)
qBytes (B x) ptr = putType (B x) ptr >>= \p -> putWord8 (unsafeCoerce x) p
qBytes (BV x) ptr = putListHeader (BV x) ptr >>= \p -> putWord8V (unsafeCoerce x) p
qBytes (X x) ptr = putType (X x) ptr >>= \p -> putWord8 (unsafeCoerce x) p
qBytes (XV x) ptr = putListHeader (XV x) ptr >>= \p -> putWord8V (unsafeCoerce x) p
qBytes (H x) ptr = putType (H x) ptr >>= \p -> putWord16 (unsafeCoerce x) p
qBytes (HV x) ptr = putListHeader (HV x) ptr >>= \p -> putWord16V (unsafeCoerce x) p
qBytes (I x) ptr = putType (I x) ptr >>= \p -> putWord32 (unsafeCoerce x) p
qBytes (IV x) ptr = putListHeader (IV x) ptr >>= \p -> putWord32V (unsafeCoerce x) p
qBytes (J x) ptr = putType (J x) ptr >>= \p -> putWord64 (unsafeCoerce x) p
qBytes (JV x) ptr = putListHeader (JV x) ptr >>= \p -> putWord64V (unsafeCoerce x) p
qBytes (E x) ptr = putType (E x) ptr >>= \p -> putWord32 (unsafeCoerce x) p
qBytes (EV x) ptr = putListHeader (EV x) ptr >>= \p -> putWord32V (unsafeCoerce x) p
qBytes (F x) ptr = putType (F x) ptr >>= \p -> putWord64 (unsafeCoerce x) p
qBytes (FV x) ptr = putListHeader (FV x) ptr >>= \p -> putWord64V (unsafeCoerce x) p
qBytes (C x) ptr = putType (C x) ptr >>= \p -> putWord8 (unsafeCoerce x) p
qBytes (CV x) ptr = putListHeader (CV x) ptr >>= \p -> putWord8V (unsafeCoerce x) p
qBytes (S x) ptr = putType (S x) ptr >>= \p -> putWord8V (unsafeCoerce x) p
qBytes (SV n x) ptr = putListHeader (SV n x) ptr >>= \p -> putWord8V (unsafeCoerce x) p
qBytes (L x) ptr = putListHeader (L x) ptr >>= \p -> V.foldM' (flip qBytes) p x
qBytes (T n x) ptr = putType (T n x) ptr >>= \p1 -> putAttr p1 >>= \p2 -> poke p2 (99 :: GHC.Word.Word8) >> V.foldM' (flip qBytes) (p2 `plusPtr` 1) x 

qIPCBytes :: Word8 -> Int -> Elems -> Ptr Word8 -> IO()
qIPCBytes mode size qobj ptr = do
       -- pack 1,mode,0,0 - all Word8
       poke ptr (1::GHC.Word.Word8)
       poke (ptr `plusPtr` 1) mode
       poke (ptr `plusPtr` 2) (0::GHC.Word.Word8)
       poke (ptr `plusPtr` 3) (0::GHC.Word.Word8)
       -- put total IPC byte length
       p1 <- putWord32 (unsafeCoerce ((fromIntegral size) :: GHC.Int.Int32)) (ptr `plusPtr` 4) 
       -- generate IPC bytes for Q object
       qBytes qobj p1
       return ()

qIPC :: Word8 -> Elems -> ByteString
qIPC x y = let bsize = 8 + size y in unsafeCreate bsize (qIPCBytes x bsize y)  
{-# INLINE qIPC #-}

fillS :: [[CChar]] -> Elems
fillS x = let (x',y') = createS x 
            in SV x' y' 
{-# INLINE fillS #-}

createS :: [[CChar]] -> (Int, SV.Vector CChar)
createS cl = unsafePerformIO $ do
            v <- MSV.new (Prelude.length . Prelude.concat $ cl)
            fill v 0 $ Prelude.concat cl
            SV.unsafeFreeze v >>= \x -> return (Prelude.length cl,x)
          where
            fill v _ [] = return ()
            fill v i (x:xs) = MSV.unsafeWrite v i x >> fill v (i + 1) xs

-- | Constructor for T - a Q table - we must always build it using this function
fillT :: V.Vector Elems -> Elems
fillT !xs = T (V.foldl' (\x y -> x + size y) 3 xs) xs -- 2 bytes for table header - 1 additional byte for dict type header. T is of the form T (dict type + list of columns) (General List - nested lists, one per column)
{-# INLINE fillT #-}

-- | Write a Word16 in little endian format
putWord8 :: Word8 -> Ptr Word8 -> IO (Ptr Word8)
putWord8 w p = poke p w >> return (p `plusPtr` 1)
{-# INLINE putWord8 #-}

-- | Write a Word16 in little endian format
putWord16 :: Word16 -> Ptr Word8 -> IO (Ptr Word8)
putWord16 w p = do
    poke p               (fromIntegral (w)          :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftR w 8) :: Word8)
    return (p `plusPtr` 2)
{-# INLINE putWord16 #-}

-- | Write a Word32 in little endian format
putWord32 :: Word32 -> Ptr Word8 -> IO (Ptr Word8)
putWord32 w p = do
  poke p (fromIntegral (w) :: Word8)
  poke (p `plusPtr` 1) (fromIntegral (shiftR w  8)  :: Word8)
  poke (p `plusPtr` 2) (fromIntegral (shiftR w  16) :: Word8)
  poke (p `plusPtr` 3) (fromIntegral (shiftR w  24) :: Word8)
  return (p `plusPtr` 4)
{-# iNLINE putWord32 #-}

-- | Write a Word64 in little endian format
putWord64 :: Word64 -> Ptr Word8 -> IO (Ptr Word8)
putWord64 w p = do
  poke p               (fromIntegral (w)           :: Word8)
  poke (p `plusPtr` 1) (fromIntegral (shiftR w  8) :: Word8)
  poke (p `plusPtr` 2) (fromIntegral (shiftR w 16) :: Word8)
  poke (p `plusPtr` 3) (fromIntegral (shiftR w 24) :: Word8)
  poke (p `plusPtr` 4) (fromIntegral (shiftR w 32) :: Word8)
  poke (p `plusPtr` 5) (fromIntegral (shiftR w 40) :: Word8)
  poke (p `plusPtr` 6) (fromIntegral (shiftR w 48) :: Word8)
  poke (p `plusPtr` 7) (fromIntegral (shiftR w 56) :: Word8)
  return (p `plusPtr` 8)
{-# INLINE putWord64 #-}

-- | Function to generate putWord<N>V functions, N = 8,16,32,64
genFnPutWordNV :: (SV.Storable a) => (a -> Ptr Word8 -> IO (Ptr Word8)) -> SV.Vector a ->  Ptr Word8 -> IO (Ptr Word8)
genFnPutWordNV f w p = SV.foldM' (flip f) p w
{-# INLINE genFnPutWordNV #-}

putWord8V :: SV.Vector Word8 -> Ptr Word8 -> IO (Ptr Word8)
putWord8V = genFnPutWordNV putWord8
{-# INLINE putWord8V #-}

putWord16V :: SV.Vector Word16 -> Ptr Word8 -> IO (Ptr Word8)
putWord16V = genFnPutWordNV putWord16
{-# INLINE putWord16V #-}

putWord32V :: SV.Vector Word32 -> Ptr Word8 -> IO (Ptr Word8)
putWord32V = genFnPutWordNV putWord32
{-# INLINE putWord32V #-}

putWord64V :: SV.Vector Word64 -> Ptr Word8 -> IO (Ptr Word8)
putWord64V = genFnPutWordNV putWord64
{-# INLINE putWord64V #-}

-- function to convert list of bytestring into hex digits - useful for debugging kx IPC bytes
bprint :: ByteString -> String
bprint x = ("0x" ++ ) $ foldl (++) "" $ fmap (printf "%02x") $ unpack x

main = do
  let shortv = HV (SV.fromList[1..10]) -- short list - for benchmark testing
      il1 = IV $ SV.enumFromN 1 5000000 -- list of int
      il2 = IV $ SV.enumFromN 1 5000000 -- list of int
      il3 = IV $ SV.enumFromN 1 5000000 -- list of int
      l1 = L (V.fromList [il1,il2,il3]) -- general list of int
      sl1 = fillS [[97,0],[98,0],[99,0]] -- symbol list: `a`b`c
      t = fillT (V.fromList [sl1,l1]) -- table t:([] a:til 5000000;b:til 5000000;c:til 5000000)
      sl2 = S (SV.fromList [116,0]) -- atomic symbol `t
      cl1 = CV (SV.fromList[46,117,46,105,110,115,101,114,116]) -- string ".u.insert"
      gl1 = (L (V.fromList [cl1,sl2,t])) -- general list (".u.insert";`t;t). t is the table from above
{--
  defaultMain [
        bench "ShortV" $ whnf SV.fromList ([1..10]::[Int16])
        ,bench "qIPC ShortV" $ whnf (qIPC 0) (HV (SV.fromList[1..10])) 
        --,bench "foldl T" $ whnf (V.foldl' (\x y -> x + size y) 3) (V.fromList [sl1,l1]) 
        ,bench "fillT" $ whnf fillT (V.fromList [sl1,l1]) 
        ,bench "fillS" $ whnf fillS [[97,0],[98,0],[99,0]] 
        ,bench "qIPC Table" $ whnf (qIPC 0) t
        ,bench "qIPC General List" $ whnf (qIPC 0) gl1
    ]
--}
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "2001")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  sendAll sock $ C.pack "user:pwd\1\0"
  msg <- recv sock 1024
  print "Received authentication"
  CM.forM_ [1..1] $ \x -> sendAll sock $ qIPC 0 gl1
  sClose sock
