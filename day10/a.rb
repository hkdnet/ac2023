lines = []
while (line = gets)
  lines << line.chomp
end

def find_s(lines)
  lines.each.with_index do |line, x|
    y = line.index("S")
    return [x, y] if y
  end
  raise "not found"
end

def add(p1, p2)
  x, y = p1
  xx, yy = p2
  [x + xx, y + yy]
end

H = lines.size
W = lines[0].size
pick = ->(point) {
  x, y = point
  if x < 0 || y < 0
    next nil
  end

  line = lines[x]
  if line
    line[y]
  else
    nil
  end
}

s = find_s(lines)
puts "starting at (#{s[0]}, #{s[1]})"
dirs = %i[N E W S]
dir_to_vec = {
  N: [-1, 0],
  E: [0, 1],
  W: [0, -1],
  S: [1, 0],
}

char_to_dirs = {
  '.' => {},
  '|' => { N: :S, S: :N },
  '-' => { E: :W, W: :E },
  'L' => { N: :E, E: :N },
  'J' => { N: :W, W: :N },
  '7' => { S: :W, W: :S },
  'F' => { S: :E, E: :S },
}
opposite = ->(d) {
  case d
  when :N
    :S
  when :S
    :N
  when :E
    :W
  when :W
    :E
  else
    raise "#{d}"
  end
}

arr = dirs.map {|dir| [add(s, dir_to_vec.fetch(dir)), dir] }.filter { |point, d|
  c = pick[point]
  next false unless c
  char_to_dirs[c][opposite[d]]
}

def c_to_h(c)
  case c
  when '|'
    { S: :S, N: :N }
  when '-'
    { W: :W, E: :E }
  when 'L'
    { S: :E, W: :N }
  when 'J'
    { S: :W, E: :N }
  when '7'
    { N: :W, E: :S }
  when 'F'
    { N: :E, W: :S }
  else
    raise c
  end
end

ans = 1

until arr.size == 2 && arr[0][0] == arr[1][0]
  tmp = []
  arr.each do |cur, dir|
    c = pick[cur]
    raise "nya-" if c.nil? || c == '.'
    to_dir = c_to_h(c).fetch(dir)
    to = add(cur, dir_to_vec.fetch(to_dir))
    tmp << [to, to_dir]
  rescue KeyError
  end
  ans += 1
  arr = tmp
end

puts ans
