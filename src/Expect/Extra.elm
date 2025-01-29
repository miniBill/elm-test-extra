module Expect.Extra exposing
    ( expectEqualMultiline
    , similarIfSmallDiff, similarIfSame, similarIfSameTrimming, similarIfSameIgnoringSpaces, similarIf, SimilarIf
    , diffMultiline
    )

{-|

@docs expectEqualMultiline
@docs similarIfSmallDiff, similarIfSame, similarIfSameTrimming, similarIfSameIgnoringSpaces, similarIf, SimilarIf
@docs diffMultiline

-}

import Ansi.Color
import Diff
import Expect
import List.Extra


{-| A version of `Expect.equal` for multiline strings.

Note: the similarity check is only used to improve the diff. `expectEqualMultiline` checks if the strings are identical or not, and will only pass if they are

-}
expectEqualMultiline : { similarIf : SimilarIf, context : Int } -> String -> String -> Expect.Expectation
expectEqualMultiline config exp actual =
    if exp == actual then
        Expect.pass

    else
        let
            header : String
            header =
                Ansi.Color.fontColor Ansi.Color.blue "Diff from expected to actual:\n"
        in
        Expect.fail (header ++ diffMultiline config exp actual)


{-| Only consider two lines similar if they are the same.
-}
similarIfSame : SimilarIf
similarIfSame =
    SimilarIf (\_ _ -> Nothing)


{-| Consider two lines similar if they are the same ignoring leading/trailing whitespace.
-}
similarIfSameTrimming : SimilarIf
similarIfSameTrimming =
    SimilarIf
        (\( l, _ ) ( r, _ ) ->
            if String.trim l == String.trim r then
                Just (Diff.diff (String.toList l) (String.toList r))

            else
                Nothing
        )


{-| Consider two lines similar if they are the same ignoring all spaces, both leading/trailing and within the line.
-}
similarIfSameIgnoringSpaces : SimilarIf
similarIfSameIgnoringSpaces =
    SimilarIf
        (\( l, ls ) ( r, rs ) ->
            if ls == rs then
                Just (Diff.diff (String.toList l) (String.toList r))

            else
                Nothing
        )


{-| Consider two lines similar if their diff is less than 1/4 of the total length.

This is more expensive to compute than the other conditions, but will produce the best diffs.

-}
similarIfSmallDiff : SimilarIf
similarIfSmallDiff =
    SimilarIf
        (\( l, ls ) ( r, rs ) ->
            if
                (ls == rs)
                    || (List.Extra.count isChange (Diff.diff ls rs)
                            <= (min (String.length l) (String.length r) // 4)
                       )
            then
                Just (Diff.diff (String.toList l) (String.toList r))

            else
                Nothing
        )


{-| Custom similarity check.
-}
similarIf : (String -> String -> Bool) -> SimilarIf
similarIf f =
    SimilarIf
        (\( l, _ ) ( r, _ ) ->
            if f l r then
                Just (Diff.diff (String.toList l) (String.toList r))

            else
                Nothing
        )


{-| Represents a check for similarity.
-}
type SimilarIf
    = SimilarIf (( String, List Char ) -> ( String, List Char ) -> Maybe (List (Diff.Change Never Char)))


type alias NonEmptyList a =
    ( a, List a )


{-| Calculate the diff between two multiline strings.
-}
diffMultiline : { similarIf : SimilarIf, context : Int } -> String -> String -> String
diffMultiline config from to =
    let
        (SimilarIf areSimilar) =
            config.similarIf

        groups :
            List
                (NonEmptyList
                    (Diff.Change
                        (List (Diff.Change Never Char))
                        ( String, List Char )
                    )
                )
        groups =
            Diff.diffWith areSimilar
                (prepare from)
                (prepare to)
                |> gatherGroups

        groupCount : Int
        groupCount =
            List.length groups
    in
    groups
        |> List.indexedMap
            (\i ( head, tail ) ->
                case head of
                    Diff.NoChange _ ->
                        if i == 0 then
                            List.reverse (List.take config.context (List.reverse tail))

                        else if i == groupCount - 1 then
                            head
                                :: List.take (config.context - 1) tail

                        else if List.length tail > 2 * config.context then
                            head
                                :: List.take (config.context - 1) tail
                                ++ Diff.NoChange ( "", [] )
                                :: Diff.NoChange ( "---", [] )
                                :: Diff.NoChange ( "", [] )
                                :: List.reverse (List.take config.context (List.reverse tail))

                        else
                            head :: tail

                    _ ->
                        head :: tail
            )
        |> List.concat
        |> List.map changeToString
        |> String.join "\n"


prepare : String -> List ( String, List Char )
prepare from =
    let
        squish : String -> List Char
        squish s =
            s
                |> String.trim
                |> String.replace " " ""
                |> String.toList
    in
    from
        |> String.lines
        |> List.map (\line -> ( line, squish line ))


changeToString : Diff.Change (List (Diff.Change Never Char)) ( String, a ) -> String
changeToString change =
    case change of
        Diff.NoChange ( before, _ ) ->
            " " ++ before

        Diff.Similar _ _ diff ->
            lineChangeToString diff

        Diff.Added ( line, _ ) ->
            Ansi.Color.fontColor Ansi.Color.green ("+" ++ line)

        Diff.Removed ( line, _ ) ->
            Ansi.Color.fontColor Ansi.Color.red ("-" ++ line)


lineChangeToString : List (Diff.Change Never Char) -> String
lineChangeToString diff =
    let
        ( befores, afters ) =
            diff
                |> List.foldr
                    (\change ( beforeAcc, afterAcc ) ->
                        case change of
                            Diff.Similar _ _ ever ->
                                never ever

                            Diff.NoChange c ->
                                let
                                    s : String
                                    s =
                                        String.fromChar c
                                in
                                ( s :: beforeAcc
                                , s :: afterAcc
                                )

                            Diff.Added a ->
                                let
                                    s : String
                                    s =
                                        String.fromChar a
                                            |> Ansi.Color.backgroundColor Ansi.Color.green
                                in
                                ( beforeAcc
                                , s :: afterAcc
                                )

                            Diff.Removed r ->
                                let
                                    s : String
                                    s =
                                        String.fromChar r
                                            |> Ansi.Color.backgroundColor Ansi.Color.red
                                in
                                ( s :: beforeAcc
                                , afterAcc
                                )
                    )
                    ( [], [] )
    in
    Ansi.Color.fontColor Ansi.Color.red ("-" ++ String.concat befores)
        ++ "\n"
        ++ Ansi.Color.fontColor Ansi.Color.green ("+" ++ String.concat afters)


gatherGroups : List (Diff.Change similar a) -> List ( Diff.Change similar a, List (Diff.Change similar a) )
gatherGroups list =
    List.Extra.groupWhile (\l r -> isChange l == isChange r) list


isChange : Diff.Change similar a -> Bool
isChange c =
    case c of
        Diff.NoChange _ ->
            False

        _ ->
            True
