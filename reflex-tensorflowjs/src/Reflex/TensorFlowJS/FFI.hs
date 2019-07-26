{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Reflex.TensorFlowJS.FFI where

import "lens"         Control.Lens hiding (element, (#))
import "data-default" Data.Default (Default, def)
import "aeson"        Data.Aeson (Value(..))
import "jsaddle"      Language.Javascript.JSaddle
--import              Reflex.TensorFlowJS.Types

newtype Model   = Model   { unModel  :: JSVal }
newtype Layers  = Layers  { unLayers :: JSVal }
newtype Tensor  = Tensor  { unTensor :: JSVal }
newtype Promise = Promise { unPromise :: JSVal }

instance Default Model where
    def = Model jsNull


-- Model APIs
sequential :: JSM Model
sequential = do
    tf    <- jsg "tf"
    model <- tf ^. js0 "sequential"
    return $ Model model

add :: Model -> Layers -> JSM ()
add (Model model) (Layers layers) = do
    _ <- model ^. js1 "add" layers
    return ()

compile :: Model -> Value -> JSM ()
compile (Model model) config = do
    _ <- model ^. js1 "compile" config
    return ()

fit :: Model
    -> Tensor
    -> Tensor
    -> JSM Promise
fit (Model model)
    (Tensor xs)
    (Tensor ys) = do
    promise <- model ^. js2 "fit" xs ys
    return $ Promise promise


predict :: Model
        -> Tensor
        -> JSM Tensor
predict (Model model) (Tensor xs) = do
    result <- model ^. js1 "predict" xs
    return $ Tensor result


-- Layers APIs
dense :: Value -> JSM Layers
dense config = do
    tf     <- jsg "tf"
    result <- tf ^. js "layers" ^. js1 "dense" config
    return $ Layers result

-- Tensor APIs
tensor2d :: [Double] -> [Double] -> JSM Tensor
tensor2d xs ys = do
    tf     <- jsg "tf"
    result <- tf ^. js2 "tensor2d" xs ys
    return $ Tensor result

-- Promise APIs
then_ :: Promise
      -- ^ Promise
      -> (JSM ())
      -- ^ Callback
      -> JSM ()
then_ (Promise promise) callback = do
    _ <- promise ^. js1 "then" callback
    return ()


-- Test
-- https://github.com/tensorflow/tfjs#via-script-tag
example :: JSM ()
example = do
    model  <- sequential
    layers <- dense Null
    model `add` layers
    model `compile` Null
    xs <- tensor2d [1,2,3,4] [4, 1]
    ys <- tensor2d [1,3,5,7] [4, 1]
    p <- fit model xs ys
    then_ p $ do
        t <- tensor2d [5] [1,1]
        (Tensor result) <- predict model t
        _ <- result ^. js0 "print"
        return ()
