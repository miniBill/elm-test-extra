module Expect.Extra exposing
    ( expectEqualMultiline
    , similarIfSmallDiff, similarIfSame, similarIfSameTrimming, similarIfSameIgnoringSpaces, similarIf, SimilarIf
    , diff, diffToString, LineDiff
    )

{-|

@docs expectEqualMultiline


# Options

@docs similarIfSmallDiff, similarIfSame, similarIfSameTrimming, similarIfSameIgnoringSpaces, similarIf, SimilarIf


# Lower level API

@docs diff, diffToString, LineDiff

-}

import Ansi.Color
import Diff
import Expect
import List.Extra


{-| A version of `Expect.equal` for multiline strings.

`context` is the number of lines of context to show in the message.

Note: the similarity check is only used to improve the diff. `expectEqualMultiline` checks if the strings are identical or not, and will only pass if they are.

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

            diffString : String
            diffString =
                diff config.similarIf exp actual
                    |> diffToString { context = config.context }
        in
        Expect.fail (header ++ diffString)


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
    = SimilarIf (( String, List Char ) -> ( String, List Char ) -> Maybe (List LineDiff))


{-| The difference between two single lines.
-}
type alias LineDiff =
    Diff.Change Never Char


{-| Calculate the diff between two multiline strings.
-}
diff : SimilarIf -> String -> String -> List (Diff.Change (List LineDiff) String)
diff areSimilar from to =
    diffInternal areSimilar from to
        |> List.map
            (\change ->
                case change of
                    Diff.Added ( a, _ ) ->
                        Diff.Added a

                    Diff.Removed ( r, _ ) ->
                        Diff.Removed r

                    Diff.NoChange ( n, _ ) ->
                        Diff.NoChange n

                    Diff.Similar ( b, _ ) ( a, _ ) d ->
                        Diff.Similar b a d
            )


diffInternal : SimilarIf -> String -> String -> List (Diff.Change (List LineDiff) ( String, List Char ))
diffInternal (SimilarIf areSimilar) from to =
    Diff.diffWith areSimilar
        (prepare from)
        (prepare to)


{-| Converts a diff between strings into a pretty-printed, colored, formatted diff.

`context` is the number of lines of context to show in the diff.

-}
diffToString : { context : Int } -> List (Diff.Change (List LineDiff) String) -> String
diffToString { context } diffLines =
    let
        groups : List ( Diff.Change (List LineDiff) String, List (Diff.Change (List LineDiff) String) )
        groups =
            gatherGroups diffLines

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
                            List.reverse (List.take context (List.reverse tail))

                        else if i == groupCount - 1 then
                            head
                                :: List.take (context - 1) tail

                        else if List.length tail > 2 * context then
                            head
                                :: List.take (context - 1) tail
                                ++ Diff.NoChange ""
                                :: Diff.NoChange "---"
                                :: Diff.NoChange ""
                                :: List.reverse (List.take context (List.reverse tail))

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


changeToString : Diff.Change (List LineDiff) String -> String
changeToString change =
    case change of
        Diff.NoChange before ->
            " " ++ before

        Diff.Similar _ _ d ->
            lineChangeToString d

        Diff.Added line ->
            Ansi.Color.fontColor Ansi.Color.green ("+" ++ line)

        Diff.Removed line ->
            Ansi.Color.fontColor Ansi.Color.red ("-" ++ line)


lineChangeToString : List LineDiff -> String
lineChangeToString diffLines =
    let
        ( befores, afters ) =
            diffLines
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
