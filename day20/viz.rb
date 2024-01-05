# frozen_string_literal: true

s = File.read(ARGV.first).chomp

h = {}
s.split("\n").each do |line|
  lhs, rhs = line.split(" -> ", 2)
  lhs.strip!
  destinations = rhs.split(",").map(&:strip)
  if lhs == "broadcaster"
    h[lhs] = { type: :b, destinations: }
  elsif lhs.start_with?("%")
    h[lhs[1..]] = { type: :ff, state: false, destinations: }
  elsif lhs.start_with?("&")
    h[lhs[1..]] = { type: :conj, from: {}, destinations: }
  else
    raise "unknown type: #{lhs.inspect}"
  end
end
tests = []
h.each do |k, v|
  v[:destinations].each do |dest|
    if h[dest].nil?
      tests << dest
      next
    end
    if h[dest][:type] == :conj
      h[dest][:from][k] = :low
    end
  end
end

tests.each { |k| h[k] = { type: :test, destinations: [] } }

puts "digraph A {"
h.each do |k, v|
  v[:destinations].each do |dest|
    puts %Q{  #{k} -> #{dest};}
  end
  col =
    case v[:type]
    when :b
      "black"
    when :ff
      "red"
    when :conj
      "blue"
    else
      "black"
    end
  puts %Q!  #{k}[color="#{col}"];!
end
puts "}"

