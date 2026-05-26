module Library where

import PdePreludat

data Ingrediente
  = Carne
  | Pan
  | Panceta
  | Cheddar
  | Pollo
  | Curry
  | QuesoDeAlmendras
  | Papas
  | PatiVegano
  | PanIntegral
  | BaconDeTofu
  deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo = 10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente PanIntegral = 3
precioIngrediente BaconDeTofu = 12

data Hamburguesa = Hamburguesa
  { precioBase :: Number,
    ingredientes :: [Ingrediente]
  }
  deriving (Eq, Show)

-- PARTE 1
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

precioFinal :: Hamburguesa -> Number
precioFinal (Hamburguesa precio lista) = precio + sum (map precioIngrediente lista)

agrandar :: Hamburguesa -> Hamburguesa
agrandar (Hamburguesa precio lista)
  | elem Carne lista = Hamburguesa precio (Carne : lista)
  | elem Pollo lista = Hamburguesa precio (Pollo : lista)
  | elem PatiVegano lista = Hamburguesa precio (PatiVegano : lista)
  | otherwise = Hamburguesa precio lista

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingredienteNuevo (Hamburguesa precioBase lista) = Hamburguesa precioBase (ingredienteNuevo : lista)

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje h = h {precioBase = precioBase h * (1 - porcentaje / 100)}

pdepBurger = (descuento 20 . agregarIngrediente Cheddar . agregarIngrediente Panceta . agrandar . agrandar) cuartoDeLibra

-- PARTE 2
dobleCuarto = agregarIngrediente Cheddar (agrandar cuartoDeLibra)

bigPdep = agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia h = (descuento 30 . agregarIngrediente Papas) h

-- PARTE 3
cambiarIngredientes :: Ingrediente -> Ingrediente
cambiarIngredientes Carne = PatiVegano
cambiarIngredientes Pollo = PatiVegano
cambiarIngredientes Cheddar = QuesoDeAlmendras
cambiarIngredientes Panceta = BaconDeTofu
cambiarIngredientes x = x

cambiarPan :: Ingrediente -> Ingrediente
cambiarPan Pan = PanIntegral
cambiarPan x = x

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie (Hamburguesa precio lista) = Hamburguesa precio (map cambiarIngredientes lista)

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati (Hamburguesa precio lista) = Hamburguesa precio (map cambiarPan lista)

dobleCuartoVegano = (cambiarPanDePati . hacerVeggie) dobleCuarto
