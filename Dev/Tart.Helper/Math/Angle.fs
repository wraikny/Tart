module wraikny.Tart.Helper.Math.Angle

[<CompiledName "Pi">]
let pi = float32 System.Math.PI

[<CompiledName "DegreeToRadian">]
let inline degreeToRadian degree =
    degree * System.Math.PI / 180.0