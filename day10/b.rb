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

lines.map! do |line|
  "." + line + "."
end
lines.unshift("." * lines[0].size)
lines << "." * lines[0].size

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

s_info = []
arr = dirs.map {|dir| [add(s, dir_to_vec.fetch(dir)), dir] }.filter { |point, d|
  c = pick[point]
  next false unless c
  ret = char_to_dirs[c][opposite[d]]
  if ret
    s_info << d
  end
  ret
}
s_info.sort!
case s_info
when [:E, :S]
  sx, sy = s
  lines[sx][sy] = 'F'
else
  raise "Unknown start: #{s_info.inspect}"
end

walls = Hash.new {|h, k| h[k] = {} }
mark = ->(point) {
  x, y = point
  walls[x][y] = true
}
mark[s]

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

until arr.size == 2 && arr[0][0] == arr[1][0]
  tmp = []
  arr.each do |cur, dir|
    mark[cur]
    c = pick[cur]
    raise "nya-" if c.nil? || c == '.'
    to_dir = c_to_h(c).fetch(dir)
    to = add(cur, dir_to_vec.fetch(to_dir))
    mark[to]
    tmp << [to, to_dir]
  rescue KeyError
  end

  arr = tmp
end

q = []
q << [0, 0]
h = Hash.new{|h, k| h[k] = {}}
used_p = ->(point) {
  x, y = point
  h[x].key?(y)
}
mark_used = ->(point) {
  x, y = point
  h[x][y] = true
}
available = ->(point, d) {
  x, y = point
  if x < 0 || x >= H || y < 0 || y >= W
    next false
  end
  if walls[x][y]
    c = pick[point]
    if !c_to_h(c).key?(d)
      next false
    end
  end
  !used_p[point]
}

while !q.empty?
  point = q.pop
  dir_to_vec.each do |(dir, delta)|
    nx = add(point, delta)
    if available[nx, dir]
      mark_used[nx]
      q << nx
    end
  end
  [
    [ 1,  1, 'F'],
    [ 1, -1, '7'],
    [-1,  1, 'J'],
    [-1, -1, 'L'],
  ].each do |dx, dy, cc|
    nx = add(point, [dx, dy])
    nxx, nxy = nx
    if nxx < 0 || nxx >= H || nxy < 0 || nxy >= W
      next false
    end
    if used_p[nx]
      next
    end
    if ![cc, '|', '-'].include?(pick[nx])
      mark_used[nx]
      q << nx
    end
  end
end

puts "H = #{H}, W = #{W}"
outer = 0
h.each do |_, hh|
  outer += hh.size
end
puts "outer = #{outer}"
w_size = 0
walls.each do |_, hh|
  w_size += hh.size
end
puts "w_size = #{w_size}"
ans = H * W - outer - w_size
puts ans

(0...H).each do |x|
  (0...W).each do |y|
    if h[x][y]
      print "O"
    elsif walls[x][y]
      print pick[[x, y]]
    else
      print "I"
    end
  end
  puts ""
end
