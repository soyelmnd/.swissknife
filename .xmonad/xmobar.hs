Config {
  font = "-*-Fixed-Bold-R-Normal-*-12-*-*-*-*-*-*-*"
, bgColor = "black"
, fgColor = "grey"
, position = TopW L 100
, commands = [
    Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
  , Run Battery ["-t","<timeleft> (<left>%","-L","50","-H","75","-h","green","-n","yell","-l","red"] 10
  , Run ThermalZone 0 ["-t","<temp>","-L","40","-H","79","-h","red","-n","yellow","-l","green"] 10
  , Run Memory ["-t","Mem: <usedratio>%"] 10
  , Run Date "%A %Y-%m-%_d %_H:%M:%S" "date" 10
  , Run StdinReader
  ]
, sepChar = "%"
, alignSep = "}{"
, template = "%StdinReader% }{ %cpu% | %memory% | %battery% %thermal0%oC) | <fc=#f88017>%date%</fc> "
}
