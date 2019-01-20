#!/usr/bin/ruby

# Generate vectors.S, the trap/interrupt entry points.
# There has to be one entry point per interrupt number
# since otherwise there's no way for trap() to discover
# the interrupt number.

print <<EOD
# generated by vectors.pl - do not edit
# handlers
.globl alltraps
EOD

(0...256).each do |i|
  puts ".globl vector#{i}"
  puts "vector#{i}:"
  if (!(i == 8 || (i >= 10 && i <= 14) || i == 17))
    puts "  push $0"
  end
  puts "  push $#{i}"
  puts "  jmp alltraps"
end

print <<EOD

# vector table
.data
.globl vectors
vectors:
EOD

(0...256).each do |i|
  puts "  .quad vector#{i}"
end
