open Core_kernel
module U = Display_util_internal

module Display_options = struct
  type t =
    { internal_options : U.Display_options.t
    ; single_column : bool
    }
  [@@deriving fields, sexp_of]

  let create ?collapse_threshold ?num_shown ?(single_column = false) () =
    { single_column
    ; internal_options = U.Display_options.create ?collapse_threshold ?num_shown ()
    }
  ;;

  let default = create ()
end

let hide_message ~num_hidden = sprintf "...%d unchanged lines..." num_hidden
let all_hidden_message = "(no changes)"

let two_column_display_as_list ?display_options diff ~on_full_width_message ~on_line_pair
  =
  let lines = U.hideable_line_pairs ?display_options diff in
  let length ~project =
    List.map lines ~f:(function
      | Hidden num_hidden -> String.length (hide_message ~num_hidden)
      | All_hidden -> String.length all_hidden_message
      | Line_pair x -> U.Line.length (project x))
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  let left_length = length ~project:U.Line_pair.fst in
  let right_length = length ~project:U.Line_pair.snd in
  let pad_to_left = left_length + 2 in
  let pad_to_right = right_length in
  let width = pad_to_left + pad_to_right in
  List.map lines ~f:(function
    | Hidden num_hidden -> on_full_width_message (hide_message ~num_hidden) ~width
    | All_hidden -> on_full_width_message all_hidden_message ~width
    | Line_pair line_pair ->
      let left = U.Line_pair.fst line_pair in
      let right = U.Line_pair.snd line_pair in
      let left_padding = String.make (pad_to_left - U.Line.length left) ' ' in
      let right_padding = String.make (pad_to_right - U.Line.length right) ' ' in
      on_line_pair ~left ~right ~left_padding ~right_padding)
;;

let center s ~width =
  let left_spaces = (width - String.length s) / 2 in
  let right_spaces = width - String.length s - left_spaces in
  [ String.make left_spaces ' '; s; String.make right_spaces ' ' ] |> String.concat
;;

let list_max l ~f = List.fold l ~init:0 ~f:(fun best x -> Int.max best (f x))

let display_single_column_as_string_with_custom_formatting
      ?display_options
      diff
      ~green
      ~red
      ~plain
  =
  let lines = U.hideable_line_pairs ?display_options diff in
  let to_text = U.Line.to_text ~green ~plain ~red in
  let width =
    list_max lines ~f:(function
      | All_hidden -> String.length all_hidden_message
      | Hidden num_hidden -> String.length (hide_message ~num_hidden)
      | Line_pair (Same line) -> String.length line.content
      | Line_pair (Different (left, right)) ->
        Int.max (String.length left.content) (String.length right.content))
  in
  List.bind lines ~f:(function
    | All_hidden -> [ all_hidden_message |> center ~width ]
    | Hidden num_hidden -> [ hide_message ~num_hidden |> center ~width ]
    | Line_pair (Same line) -> [ line |> to_text ]
    | Line_pair (Different (left, right)) ->
      [ left |> to_text; right |> to_text ]
      |> List.filter ~f:(String.exists ~f:(Fn.non Char.is_whitespace)))
;;

let display_two_column_as_string_with_custom_formatting
      ?display_options
      diff
      ~green
      ~red
      ~plain
  =
  let on_line_pair ~left ~right ~left_padding ~right_padding =
    [ U.Line.to_text ~green ~red ~plain left
    ; left_padding
    ; U.Line.to_text ~green ~red ~plain right
    ; right_padding
    ]
    |> String.concat
  in
  two_column_display_as_list
    ?display_options
    diff
    ~on_full_width_message:center
    ~on_line_pair
;;

let display_as_string_with_custom_formatting ?display_options diff ~green ~red ~plain =
  let single_column =
    Option.value display_options ~default:Display_options.default
    |> Display_options.single_column
  in
  let display_options = Option.map display_options ~f:Display_options.internal_options in
  let display =
    match single_column with
    | true -> display_single_column_as_string_with_custom_formatting
    | false -> display_two_column_as_string_with_custom_formatting
  in
  display ?display_options diff ~green ~red ~plain |> String.concat ~sep:"\n"
;;

let display_as_plain_string ?display_options diff =
  display_as_string_with_custom_formatting
    ?display_options
    diff
    ~green:(fun x -> "+" ^ x)
    ~red:(fun x -> "-" ^ x)
    ~plain:(fun x -> " " ^ x)
;;

let display_with_ansi_colors ?display_options diff =
  display_as_string_with_custom_formatting
    ?display_options
    diff
    ~green:(fun x -> sprintf "\027[32m%s\027[0m" x)
    ~red:(fun x -> sprintf "\027[31m%s\027[0m" x)
    ~plain:Fn.id
;;

let%expect_test "the diff looks like this" =
  let original =
    {|
      ((apple 123)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (carrot -1))
    |}
    |> Sexp.of_string
  in
  let updated =
    {|
      ((apricot 321)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (durian 1234)
       (carrot 42))
    |}
    |> Sexp.of_string
  in
  let diff = Algo.diff ~original ~updated () in
  let () = print_endline (display_as_plain_string diff) in
  [%expect
    {|
  (                         (
 - (apple 123)             + (apricot 321)
   (banana 1000)             (banana 1000)
   (banana 1000)             (banana 1000)
   (banana 1000)             (banana 1000)
             ...6 unchanged lines...
   (banana 1000)             (banana 1000)
   (banana 1000)             (banana 1000)
   (banana 1000)             (banana 1000)
                           + (durian 1234)
   (carrot                   (carrot
 -  -1                     +  42
   ))                        )) |}]
;;
