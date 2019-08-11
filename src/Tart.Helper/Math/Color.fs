﻿module wraikny.Tart.Helper.Math.Color

open wraikny.Tart.Helper.Math.Vec3
open FSharpPlus

/// 0.0f ~ 1.0f
let HSV2RGB_float32 (hsv : float32 Vec3) : float32 Vec3 =
    let { Vec3.x=h; y=s; z=v } = hsv
    let r, g, b = v, v, v

    let h = h * 6.0f
    let i = uint16 h
    let f = h - float32 i
    let r, g, b =
        i |> function
        | 1us ->
            (
                1.0f - s * f,
                g,
                1.0f - s
            )
        | 2us ->
            (
                1.0f - s,
                g,
                1.0f - s * (1.0f - f)
            )
        | 3us ->
            (
                1.0f - s,
                1.0f - s * f,
                b
            )
        | 4us ->
            (
                1.0f - s * (1.0f - f),
                1.0f - s,
                b
            )
        | 5us ->
            (
                r,
                1.0f - s,
                1.0f - s * f
            )
        | _ ->
            (
                r,
                1.0f - s * (1.0f - f),
                1.0f - s
            )

    Vec3.init r g b

/// 0 ~ 255
let HSV2RGB_byte (hsv : byte Vec3) : byte Vec3 =
    hsv
    |>> (float32 >> flip (/) 255.0f)
    |> HSV2RGB_float32
    |>> ((*) 255.0f >> byte)