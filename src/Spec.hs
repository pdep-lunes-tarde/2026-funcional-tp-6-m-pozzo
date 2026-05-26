module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 6: Hamburguesas" $ do
    
        describe "Parte 1" $ do
            it "precioFinal suma el precio base y el de todos los ingredientes" $ do
                -- cuartoDeLibra: Base 20 + Pan 2 + Carne 20 + Cheddar 10 + Pan 2 = 54
                precioFinal cuartoDeLibra `shouldBe` 54
            
            it "agrandar una hamburguesa de carne agrega un medallon de carne debajo del pan superior" $ do
                agrandar (Hamburguesa 120 [Pan, Carne, Pan]) `shouldBe` Hamburguesa 120 [Pan, Carne, Carne, Pan]
                
            it "agrandar una hamburguesa de pollo agrega un medallon de pollo" $ do
                agrandar (Hamburguesa 100 [Pan, Pollo, Pan]) `shouldBe` Hamburguesa 100 [Pan, Pollo, Pollo, Pan]
                
            it "agregarIngrediente suma el ingrediente especificado debajo del pan superior" $ do
                agregarIngrediente Panceta (Hamburguesa 50 [Pan, Carne, Pan]) `shouldBe` Hamburguesa 50 [Pan, Panceta, Carne, Pan]
                
            it "descuento aplica el porcentaje indicado unicamente al precio base de la hamburguesa" $ do
                descuento 20 (Hamburguesa 100 [Carne]) `shouldBe` Hamburguesa 80 [Carne]
                
            it "pdepBurger tiene el precio final correcto esperado de 110" $ do
                precioFinal pdepBurger `shouldBe` 110

        describe "Parte 2" $ do
            it "dobleCuarto tiene el precio final esperado de 84" $ do
                precioFinal dobleCuarto `shouldBe` 84
                
            it "bigPdep tiene el precio final esperado de 89" $ do
                precioFinal bigPdep `shouldBe` 89
                
            it "delDia aplica el 30% de descuento al precio base y agrega Papas (doble cuarto del dia vale 88)" $ do
                precioFinal (delDia dobleCuarto) `shouldBe` 88

        describe "Parte 3" $ do
            it "hacerVeggie reemplaza carne por PatiVegano, cheddar por QuesoDeAlmendras y panceta por BaconDeTofu" $ do
                hacerVeggie (Hamburguesa 20 [Pan, Carne, Cheddar, Panceta, Pan]) `shouldBe` Hamburguesa 20 [Pan, PatiVegano, QuesoDeAlmendras, BaconDeTofu, Pan]
                
            it "cambiarPanDePati cambia todos los Pan por PanIntegral" $ do
                cambiarPanDePati (Hamburguesa 10 [Pan, Carne, Pan]) `shouldBe` Hamburguesa 10 [PanIntegral, Carne, PanIntegral]
                
            it "dobleCuartoVegano es un doble cuarto con ingredientes veganos y pan integral" $ do
                -- La estructura original del dobleCuarto es: [Pan, Cheddar, Carne, Carne, Cheddar, Pan]
                dobleCuartoVegano `shouldBe` Hamburguesa 20 [PanIntegral, QuesoDeAlmendras, PatiVegano, PatiVegano, QuesoDeAlmendras, PanIntegral]
