let src = Logs.Src.create "jude.logs" ~doc:"logs jude library output"
module Log = (val Logs.src_log src)

include Log