(*
  Wow, this seems to almost actually work now.
  I'm not gonna bother with denormalized numbers, since I don't even
  know what they are.  
  Might not be a bad idea to handle 64-bit floats.  LATER.

  Simon Heath
  24/12/2005 (Yes, I'm hacking on Christmas)
*)


let string_of_char c =
   String.make 1 c
;;

let dec2bin f =
  let numBits = 31
  and numericBaseData = "01"   (* Output digits can be 0 or 1 *)
  and binaryFraction = 0.5     (* Binary fractions are powers of 0.5 *)
  and outputText = ref "" in

    (* Special case... *)
    if f = 0. then
      "0.0"
    else (

      (* Check for negatives *)
      let correctedVal = if f < 0. then -.f else f in

      let decimalValue = floor correctedVal in
      let fractionValue = correctedVal -. decimalValue in

      (* Figure out the whole part *)
      let workingDecimalValue = ref decimalValue in
	while !workingDecimalValue > 0. do
	  let intX = int_of_float (((!workingDecimalValue /. 2.) -.
				      (floor (!workingDecimalValue /. 2.)))
				   *. 2. +. 1.5) in
	    outputText := (string_of_char numericBaseData.[intX - 1]) ^ !outputText;
	    workingDecimalValue := floor (!workingDecimalValue /. 2.);
	done;

	outputText := !outputText ^ ".";

	(* Figure out the fraction part *)
	let workingFractionValue = ref fractionValue in
	let workingBinaryFraction = ref binaryFraction in

	  while ((String.length !outputText) < numBits) &&
            (!workingFractionValue > 0.) do
	      if !workingFractionValue >= !workingBinaryFraction then (
		outputText := !outputText ^ "1";
		workingFractionValue := !workingFractionValue -. !workingBinaryFraction;
	      )
	      else
		outputText := !outputText ^ "0";
	      workingBinaryFraction := !workingBinaryFraction /. 2.;
	  done;

	  if f < 0. then
	    outputText := "-" ^ !outputText;

	  !outputText )
;;

(* This only actually works for 32-bit floats, since 64-bit floats have
   a base of more than OCaml's 31-bits.
*)
type ieeefloat = {
   fpsign : int;
   fpbase : int;
   fpexpt : int;
};;

let makeIeeeFloat sign base expt = {
   fpsign = sign;
   fpbase = base;
   fpexpt = expt;
};;

(* This just turns a normal number into a binary number 
   ...doesn't do negatives.  Crap.
   ...hm.  A negative integer is (lnot i) + 1, so...
*)
let int2bin i =
   let numericBaseData = "01"   (* Output digits can be 0 or 1 *)
   and outputText = ref "" in

   let decimalValue = float_of_int i in

   (* Figure out the whole part *)
   let workingDecimalValue = ref decimalValue in
   while !workingDecimalValue > 0. do
      let intX = int_of_float (((!workingDecimalValue /. 2.) -.
                    (floor (!workingDecimalValue /. 2.)))
                    *. 2. +. 1.5) in
      outputText := (string_of_char numericBaseData.[intX - 1]) ^ !outputText;
      workingDecimalValue := floor (!workingDecimalValue /. 2.);
   done;
   !outputText
;;

let printBin i =
   print_endline ("Bin: " ^ (int2bin i))
;;


(* This chops the binary string down to the length given, or pads it out 
   This prevents overflows and such.
*)
let padBinString str len =
   let workstring = ref str in
   if (String.length str) > len then
      workstring := String.sub str 0 len
   else
      while (String.length !workstring) < len do
         workstring := !workstring ^ "0";
      done;
   !workstring
;;

(* Takes a binary number in a string and turns it into an ieee number *)
let bin2ieee str =
  (* Hokay.  We check for special cases, then negatives, save the
     exponant, normalize it, and chop off the leading 1.  *)
  (* Special cases --0, inf, nan *)
  let sign = if str.[0] = '-' then -1 else 1 in
    if str = "0.0" then (
      print_endline "Grar.";
      makeIeeeFloat sign 0 (-0x7f)
    )
    else if str = "nan" then
      makeIeeeFloat sign 1 0x80
    else if str = "inf" then
      makeIeeeFloat sign 0 0x80
    else (
      (* Check for negatives... a bit of a kludge, but what isn't? *)
      let sign = if str.[0] = '-' then -1 else 1 in
      let str = if str.[0] = '-' then 
	String.sub str 1 ((String.length str) - 1) else str
      in

      let expt = (String.index str '.') - 1 in

      let normalizeString s =
	let resString = String.make (String.length s - 2) '2' in
	let strptr = ref 1 in
	  for x = 0 to (String.length resString - 1) do
            if s.[!strptr] = '.' then
	      incr strptr;
            resString.[x] <- s.[!strptr];
	    incr strptr;
	  done;
	  resString
      in
      let str = padBinString (normalizeString str) 22 in
	(*print_endline ("Base: " ^ str);*)
      let normalizedNumber = int_of_string ("0b" ^ str) in
	makeIeeeFloat sign normalizedNumber expt
    )
;;


(* Encodes an IEEE number into a 32-bit integer value 
   ...there are some times when OCaml ints not having that 32nd bit
   kinda stings.
*)
let encodeFloat32 ie =
   let result = (ref Int32.zero) in
   (* Check negative and set the sign bit accordingly *)
   if ie.fpsign < 0 then
      result := Int32.logor !result (Int32.of_string "0x80000000");

   if (ie.fpbase > 0x800000) then
      print_endline "Squirr!"
      (* errorAndDie "Float base out of bounds of 32 bits!" *)
   else (
      result := Int32.logor !result (Int32.of_int ie.fpbase)
   );
      result := Int32.add !result (Int32.of_int ie.fpbase);
   (*printBin (Int32.to_int !result);*)

   (* Check exponent, and set the 8 exponent bits accordingly.
      Encoded exponant = true exponant + 7f *)
   if (ie.fpexpt > 128) || (ie.fpexpt < -127) then
      print_endline "Bop!"
      (* errorAndDie "Float exponant out of bounds of 32 bits!" *)
      (* XXX: We should make sure it doesn't overflow, theoretically... *)
   else (
      let expt = (ie.fpexpt + 0x7F) in
      let expt32 = Int32.of_int expt in
      (*print_string "E";
      printBin expt; *)
      result := Int32.logor !result (Int32.shift_left expt32 23)
   );

   (*print_string "R";*)
   (*printBin (Int32.to_int !result); *)


   !result;
;;

let float32Repr f =
  encodeFloat32 (bin2ieee (dec2bin f))
;;
(*
let printIeeeFloat ie =
   (*Printf.printf "%c" (if ie.fpsign < 0 then '-' else '+');
   Printf.printf "#x%X" ie.fpbase;
   Printf.printf " * 2^#x%X\n" ie.fpexpt;*)

   Printf.printf "#x%lX\n" (encodeFloat32 ie);
;;

let a = dec2bin (170141183460469231731687303715884105728.0);;
let b = bin2ieee a;;
let c = encodeFloat32 b;;

printIeeeFloat b;
*)

(* Translated from this:
Private Sub Dec2Bin_Click()

'   Subroutine to convert floating point decimal values to floating point binary values.
'
'   Written by: Erik Oosterwal
'   Started on: October 26, 2005
'   Completed:  October 27, 2005

    Dim txtNumericBaseData, txtOutputValue As String
    Dim intX, intNumBits As Integer
    Dim dblDecimalValue, dblFractionValue, dblBinaryFraction As Double
        
        
    intNumBits = 20             ' The number of bits in the output (including the decimal point)
    txtNumericBaseData = "01"   ' Output digits can be either "0" or "1"
    dblBinaryFraction = 0.5     ' Binary fraction bits are powers of 0.5
    txtOutput.Text = ""         ' Clear the display
    txtOutputValue = ""         ' Clear the working output variable
    
    
    dblDecimalValue = Int(CDbl(txtInput.Text))                  ' Get the integer portion of the input
    dblFractionValue = CDbl(txtInput.Text) - dblDecimalValue    ' Get the fractional portion of the input
        
        
'   Figure out the integer portion of the input.
    While dblDecimalValue > 0
        intX = Int(((dblDecimalValue / 2) - Int(dblDecimalValue / 2)) * 2 + 1.5)
        txtOutputValue = Mid(txtNumericBaseData, intX, 1) + txtOutputValue
        dblDecimalValue = Int(dblDecimalValue / 2)
    Wend
    
    
    If txtOutputValue = "" Then txtOutputValue = "0"    ' If there was no whole number, set it to "0"
    txtOutputValue = txtOutputValue + "."               '   then add a decimal point
    
    
'   Figure out the fractional portion of the input.
    While Len(txtOutputValue) < intNumBits And dblFractionValue > 0     ' As long as we're not exceeding the
                                                                        ' allowed number of bits in the output
                                                                        ' and there's still part of the input
                                                                        ' value to be converted...
        If dblFractionValue >= dblBinaryFraction Then                   ' If the input number is larger than the fraction bit,
            txtOutputValue = txtOutputValue + "1"                       '   add a "1" to the output and
            dblFractionValue = dblFractionValue - dblBinaryFraction     '   reduce the input number by the fraction bit's value.
        Else                                                            ' Otherwise...
            txtOutputValue = txtOutputValue + "0"                       '   just tag on a "0" to the end of the output.
        End If
        dblBinaryFraction = dblBinaryFraction / 2#                      ' The fraction bit must be cut in half.
    Wend
    
    txtOutput.Text = txtOutputValue                     ' Send the output value to the display.

End Sub

*)
