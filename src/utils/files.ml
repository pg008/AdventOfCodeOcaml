let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  let l = String.length vsebina in
  if vsebina.[l - 1] = '\n' then String.sub vsebina 0 (l - 1) else vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan (vsebina ^ "\n");
  close_out chan
