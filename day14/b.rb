# frozen_string_literal: true

s = File.read(ARGV.first).chomp

ROUND = 'O'
CUBE = '#'
EMPTY = '.'

ORDER = {
  EMPTY => 0,
  ROUND => 1
}.freeze

def fall(mat)
  mat.map do |tiles|
    fall_tile(tiles)
  end
end

def fall_tile(tiles)
  cubes = tiles.each_with_index.filter_map do |tile, idx|
    tile == CUBE ? idx : nil
  end

  cubes.unshift(-1)
  cubes << tiles.size


  cubes.each_cons(2) do |a, b|
    tiles[a+1...b] = tiles[a+1...b].sort_by { |c| ORDER.fetch(c) }
  end

  tiles
end

def rotate(mat)
  h = mat.size
  w = mat.first.size
  w.times.map do |ww|
    h.times.map do |hh|
      mat[h - hh - 1][ww]
    end
  end
end

def s2b(s)
  s.split("\n").map(&:chars)
end

def b2s(mat)
  mat.map(&:join).join("\n")
end

def cycle(mat)
  4.times do
    mat = fall(mat)
    mat = rotate(mat)
  end

  mat
end

def initial_rotate(mat)
  rotate(mat)
end

def final_rotate(mat)
  rotate(rotate(rotate(mat)))
end

def calc_score(mat)
  h = mat.size
  score = 0
  mat.each.with_index do |row, hh|
    mul = h - hh
    score += row.count(ROUND) * mul
  end
  score
end

initial = rotate(s2b(s))

# puts b2s(s2b(s))

# puts('-' * 3)
# puts b2s(initial)
# puts('-' * 3)
# puts b2s(fall(initial))
# puts('-' * 3)
memo = {}
tmp = initial
cnt = 0
memo[b2s(tmp)] = cnt

cycles = 1000000000

loop do
  cnt += 1
  tmp = cycle(tmp)
  key = b2s(tmp)
  if memo[key]
    loop_len = cnt - memo[key]
    cycles -= cnt
    cycles %= loop_len
    break
  else
    memo[key] = cnt
  end
end
cycles.times do
  tmp = cycle(tmp)
end

final = final_rotate(tmp)
puts b2s(final)

puts calc_score(final)
