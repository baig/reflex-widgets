{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import                       Prelude hiding (head)
import "lens"                Control.Lens
import "data-default"        Data.Default (def)
import "aeson"               Data.Aeson (Value(..), toJSON)
import "aeson-qq"            Data.Aeson.QQ
import "text"                Data.Text (Text, pack)
import "jsaddle"             Language.Javascript.JSaddle -- (JSVal) --  GHCJS.Types (JSVal)
import "reflex-dom"          Reflex.Dom hiding (Value)
import "reflex-utils"        Reflex.Utils
import "reflex-tensorflowjs" Reflex.TensorFlowJS

--
main :: IO ()
main = mainWidget main_
    where
        main_ :: forall t m. MonadWidget t m => m ()
        main_ = do
            headD <- head
            whenLoaded [headD] blank body
            return ()

--
head :: forall t m. MonadWidget t m => m (Dynamic t Bool)
head = do
    s1Ds <- sequence [ script "https://cdn.jsdelivr.net/npm/@tensorflow/tfjs/dist/tf.min.js"
                     ]
    whenLoaded s1Ds blank $ return ()

--
body :: MonadWidget t m => m ()
body = do

    -- fit data event
    click1E <- button "Fit"
    let fitE = (xs, ys) <$ click1E

    -- tensorflow fit
    trainedModelE <- tfFit fitE $ \(xs', ys') -> do
        xs'' <- tensor2d (fst xs') (snd xs')
        ys'' <- tensor2d (fst ys') (snd ys')
        model  <- sequential
        layers <- dense [aesonQQ| {units: 1, inputShape: [1]} |]
        model `add` layers
        compile model [aesonQQ| {loss: "meanSquaredError", optimizer: "sgd"} |]
        promise <- fit model xs'' ys''
        return (model, promise)

    -- predict data event
    click2E <- button "Predict"
    let tensorE = ps <$ click2E

    -- combine with trained model
    trainedModelD <- holdDyn def trainedModelE
    tensorD       <- holdDyn ps tensorE
    let predictD = (,) <$> trainedModelD
                       <*> tensorD
    let predictE = updated predictD

    -- tensorflow predict
    predictedE <- tfPredict predictE $ \(model, xs') -> do
        xs'' <- tensor2d (fst xs') (snd xs')
        result <- predict model xs''
        return result

    predictedD <- holdDyn (undefined, "") predictedE
    display (snd <$> predictedD)

    return ()
    where
        xs :: Tensor2D
        xs = ([1, 2, 3, 4], [4, 1])

        ys :: Tensor2D
        ys = ([1, 3, 5, 7], [4, 1])

        ps :: Tensor2D
        ps = ([5], [1,1])
