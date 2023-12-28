s = File.read(ARGV.first).chomp
ops = s.split("\n").map do |line|
  _, _, s = line.split(" ", 3)
  len = s[2..-3].to_i(16)
  dir = %w[R D L U][s[-2].to_i]
  { len:, dir: }
end
ops.each_cons(2) do |a, b|
  if a[:dir] == b[:dir]
    raise "nya-"
  end
end

def build_points(ops)
  start_at = [0, 0]
  ret = [start_at]
  tmp = start_at
  ops.each do |h|
    d = case h[:dir]
        when "R" then [0, 1]
        when "L" then [0, -1]
        when "U" then [-1, 0]
        when "D" then [1, 0]
        end
    x, y = tmp
    tmp = [x + d[0] * h[:len], y + d[1] * h[:len]]
    ret << tmp
  end

  ret
end

def compress(points)
  xs = points.flat_map {|x, _| [x-1, x, x + 1] }
  ys = points.flat_map {|_, y| [y-1, y, y + 1] }
  cx = {}
  xs.sort.uniq.each.with_index do |x, idx|
    cx[x] = idx
  end
  cy = {}
  ys.sort.uniq.each.with_index do |y, idx|
    cy[y] = idx
  end
  cs = points.map do |x, y|
    [cx[x], cy[y]]
  end

  [cx, cy, cs]
end

points = build_points(ops)
cx, cy, compressed = compress(points)
rev_cx = cx.invert
rev_cy = cy.invert

compress_x = ->(orig_x) { cx.fetch(orig_x) }
decompress_x = ->(compressed_x) { rev_cx.fetch(compressed_x) }
compress_y = ->(orig_y) { cy.fetch(orig_y) }
decompress_y = ->(compressed_y) { rev_cy.fetch(compressed_y) }

grid = Hash.new {|h, k| h[k] = {} }


start_at = [compress_x[0], compress_y[0]]
tmp = start_at
ops.each do |h|
  x, y = tmp
  r = case h[:dir]
      when "R"
        orig_target = decompress_y[y] + h[:len]
        target = compress_y[orig_target]
        (y..target).map {|y| [x, y] }
      when "L"
        orig_target = decompress_y[y] - h[:len]
        target = compress_y[orig_target]
        (target..y).map {|y| [x, y] }
      when "U"
        orig_target = decompress_x[x] - h[:len]
        target = compress_x[orig_target]
        (target..x).map {|x| [x, y] }
      when "D"
        orig_target = decompress_x[x] + h[:len]
        target = compress_x[orig_target]
        (x..target).map {|x| [x, y] }
      end
  r.each { |x, y| grid[x][y] = true }
  tmp = [r.first, r.last].reject { |e| e == tmp }.first
end

start_at = [compress_x[1], compress_y[1]]
grid[start_at[0]][start_at[1]] = true
q = [start_at]
until q.empty?
  x, y = q.shift

  [
    [1, 0],
    [-1, 0],
    [0, 1],
    [0, -1],
  ].each do |dx, dy|
    xx = x + dx
    yy = y + dy
    next if xx < 0 || yy < 0
    next if grid[xx][yy]

    grid[xx][yy] = true
    q << [xx, yy]
  end
end


ans = 0
xs = cx.values.sort
ys = cy.values.sort
xs.each do |x|
  ys.each do |y|
    if grid[x][y]
      h = decompress_x[x+1]-decompress_x[x]
      w = decompress_y[y+1] - decompress_y[y]
      ans += h*w
    end
  end
end
puts ans
