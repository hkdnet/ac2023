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

low_count = 0
high_count = 0

push_button = ->() {
  q = [{ dest: "broadcaster", pulse: :low, from: "button" }]
  until q.empty?
    a = q.shift
    puts "#{a[:from]} -#{a[:pulse]}-> #{a[:dest]}"
    from = a[:from]
    dest = a[:dest]
    pulse = a[:pulse]
    if pulse == :high
      high_count += 1
    else
      low_count += 1
    end
    cur = h[dest]
    to_pulse =
      case cur[:type]
      when :ff
        next if pulse == :high
        cur[:state] = !cur[:state]
        cur[:state] ? :high : :low
      when :conj
        cur[:from][from] = pulse
        cur[:from].values.all? { |v| v == :high } ? :low : :high
      else
        pulse
      end
    cur[:destinations].each do |new_dest|
      q << { dest: new_dest, pulse: to_pulse, from: dest }
    end
  end
}

1000.times {
  push_button.call
}
p high_count
p low_count
puts high_count * low_count
