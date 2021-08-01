BEGIN {
  FS = "\t"
  print "digraph pigy {"
  print "rankdir=TB"
  print "node [labelloc=t shape=box]"
}

{
  self = $1
  name = $2
  split(gensub("@", "_", "g", $3), parents, ",")
  node = gensub("@", "_", "g", self)
  print ""
  print node " [label=\"" name "\" image=\"images/" self ".png\" href=\"https://pool.pm/cbf096ed812bdafc8b000886cf7b1ccd4e430e78dc579c7f25a155d3." self "\"]"
  for (i in parents)
    print parents[i] " -> " node
}

END {
  print ""
  print "}"
}
