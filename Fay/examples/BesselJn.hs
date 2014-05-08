{-# LINE 1 "BesselJn.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "BesselJn.hsc" #-}
 
module Bessel (besselJn) where
 
import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
 

{-# LINE 11 "BesselJn.hsc" #-}
 
data GslSfResult = GslSfResult { gsl_value :: CDouble, gsl_error :: CDouble }
 
instance Storable GslSfResult where
    sizeOf    _ = ((16))
{-# LINE 16 "BesselJn.hsc" #-}
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        value <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 19 "BesselJn.hsc" #-}
        error <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 20 "BesselJn.hsc" #-}
        return  GslSfResult { gsl_value = value, gsl_error = error }
    poke ptr (GslSfResult value error) = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr value
{-# LINE 23 "BesselJn.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr error
{-# LINE 24 "BesselJn.hsc" #-}

besselJn :: Int -> Double -> Either String (Double, Double)
besselJn n x = unsafePerformIO $
    alloca $ \gslSfPtr -> do
        c_deactivate_gsl_error_handler
        status <- besselJn (fromIntegral n) (realToFrac x) gslSfPtr
        if status == 0
            then do
                GslSfResult val err <- peek gslSfPtr
                return $ Right (realToFrac val, realToFrac err)
            else do
                error <- c_error_string status
                error_message <- peekCString error
                return $ Left ("GSL error: "++error_message)

foreign import ccall unsafe "gsl/gsl_bessel.h gsl_sf_bessel_Jn_e"
     c_besselJn :: CInt -> CDouble -> Ptr GslSfResult -> IO CInt
 
foreign import ccall unsafe "gsl/gsl_errno.h gsl_set_error_handler_off"
     c_deactivate_gsl_error_handler :: IO ()
 
foreign import ccall unsafe "gsl/gsl_errno.h gsl_strerror"
     c_error_string :: CInt -> IO CString

