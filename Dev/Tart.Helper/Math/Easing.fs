﻿namespace wraikny.Tart.Helper.Math

// https://easings.net/

type Easing =
    | Linear
    | InSine
    | OutSine
    | InOutSine
    | InQuad
    | OutQuad
    | InOutQuad
    | InCubic
    | OutCubic
    | InOutCubic
    | InQuart
    | OutQuart
    | InOutQuart
    | InQuint
    | OutQuint
    | InOutQuint
    | InExpo
    | OutExpo
    | InOutExpo
    | InCirc
    | OutCirc
    | InOutCirc
    | InBack
    | OutBack
    | InOutBack
    | InElastic
    | OutElastic
    | InOutElastic
    | InBounce
    | OutBounce
    | InOutBounce


open System
open wraikny.Tart.Helper.Math


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Easing =
    [<CompiledName "Calculate">]
    let calculate easing (frame : int) (current : int) : float32 =
        let t = (float32 current) / (float32 frame)

        if t < 0.0f then 0.0f
        elif t > 1.0f then 1.0f
        else
            let pow(a, b) = Math.Pow(float a, float b) |> float32
            easing |> function
            | Linear -> 
                t
            | InSine ->
                -cos(t * float32(Angle.degreeToRadian 90.0) )
            | OutSine ->
                sin(t * float32(Angle.degreeToRadian 90.0) )
            | InOutSine ->
                -1.0f / 2.0f * (cos(t * Angle.pi) - 1.0f)
            | InQuad ->
                t * t
            | OutQuad ->
                t * (2.0f - t)
            | InOutQuad ->
                if t < 0.5f then 2.0f * t * t else t * (4.0f - 2.0f * t) - 1.0f
            | InCubic ->
                t * t * t
            | OutCubic ->
                let t = t - 1.0f
                (t * t * t + 1.0f)
            | InOutCubic ->
                (
                    if t < 0.5f
                    then 4.0f * t * t * t
                    else
                        let t = t - 1.0f
                        1.0f + (t) * (2.0f * t) * (2.0f * t)
                )
            | InQuart ->
                t * t * t * t
            | OutQuart ->
                let t = t - 1.0f
                -(t * t * t * t - 1.0f)
            | InOutQuart ->
                if t < 0.5f then
                    let t = t * t;
                    8.0f * t * t
                else
                    let t = (t - 1.0f) * t
                    1.0f - 8.0f * t * t
            | InQuint ->
                t * t * t * t * t
            | OutQuint ->
                let t = t - 1.0f
                t * t * t * t * t + 1.0f
            | InOutQuint ->
                if t < 0.5f then
                    let t2 = t * t;
                    16.0f * t * t2 * t2
                else
                    let t2 = (t - 1.0f) * t;
                    1.0f + 16.0f * t * t2 * t2
            | InExpo ->
                if t = 0.0f then 0.0f else pow(2.0f, 10.0f * (t - 1.0f) )
            | OutExpo ->
                if t = 1.0f then 1.0f else -pow(2.0f, -10.0f * t) + 1.0f
            | InOutExpo ->
                if t = 0.0f then 0.0f
                elif t = 1.0f then 1.0f
                elif t < 0.5f then
                    (pow(2.0f, 16.0f * t) - 1.0f) / 510.0f
                else
                    1.0f - 0.5f * pow(2.0f, -16.0f * (t - 0.5f))
            | InCirc ->
                1.0f - sqrt(1.0f - t)
            | OutCirc ->
                sqrt(t)
            | InOutCirc ->
                if t < 0.5f then
                    (1.0f - sqrt(1.0f - 2.0f * t)) * 0.5f
                else
                    (1.0f + sqrt(2.0f * t - 1.0f)) * 0.5f
            | InBack ->
                t * t * (2.70158f * t - 1.70158f)
            | OutBack ->
                let t = t - 1.0f
                1.0f + t * t * (2.70158f * t + 1.70158f)
            | InOutBack ->
                if t < 0.5f then
                    t * t * (7.0f * t - 2.5f) * 2.0f
                else
                    let t = t - 1.0f
                    1.0f + t * t * 2.0f * (7.0f * t + 2.5f)
            | InElastic ->
                t * t * t * t * sin(t * Angle.pi * 4.5f)
            | OutElastic ->
                let t2 = (t - 1.0f) * (t - 1.0f)
                1.0f - t2 * t2 * cos(t * Angle.pi * 4.5f)
            | InOutElastic ->
                if t < 0.45f then
                    let t2 = t * t
                    8.0f * t2 * t2 * sin(t * Angle.pi * 9.0f)
                elif t < 0.55f then
                    0.5f + 0.75f * sin(t * Angle.pi * 4.0f)
                else
                    let t2 = (t - 1.0f) * (t - 1.0f)
                    1.0f - 8.0f * t2 * t2 * sin(t * Angle.pi * 9.0f)
            | InBounce ->
                pow(2.0f, 6.0f * (t - 1.0f)) * abs(sin(t * Angle.pi * 3.5f))
            | OutBounce ->
                1.0f - pow(2.0f, -6.0f * t) * abs(cos(t * Angle.pi * 3.5f))
            | InOutBounce ->
                if t < 0.5f then
                    8.0f * (pow(2.0f, 8.0f * (t - 1.0f))) * abs(sin(t * Angle.pi * 7.0f))
                else
                    1.0f - 8.0f * (pow(2.0f, -8.0f * t)) * abs(sin(t * Angle.pi * 7.0f))


    [<CompiledName "InterpolateVector">]
    let inline interpolateVector easing (frame) (current) (startPoint : 'Vec) (endPoint : 'Vec) : 'Vec =
        Vector.constraint' Unchecked.defaultof<Vector<'a, 'Vec>>
        let v = calculate easing frame current
        startPoint + (endPoint - startPoint) *. v

