const std = @import("std");
const mem = @import("std").mem;

fn nextToken(reader: anytype, buffer: []u8) []const u8 {
    return reader.readUntilDelimiter(
        buffer,
        ' ',
    ) catch unreachable;
}

fn nextLine(reader: anytype, buffer: []u8) []const u8 {
    const line = reader.readUntilDelimiterOrEof(
        buffer,
        '\n',
    ) catch unreachable;

    return line.?;
}

fn parseUsize(buf: []const u8) usize {
    return std.fmt.parseInt(usize, buf, 10) catch unreachable;
}
fn extract(buf: []const u8, idx: usize) ?u32 {
    const c = buf[idx];

    if (c >= '0' and c <= '9') {
        return c - '0';
    }
    var literals = [_][]const u8{
        "one",
        "two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine",
    };
    for (literals, 1..) |literal, val| {
        const r = idx + literal.len;
        if (r > buf.len) {
            continue;
        }
        if (mem.eql(u8, buf[idx .. idx + literal.len], literal)) {
            return @intCast(val);
        }
    }

    return null;
}

pub fn main() !void {
    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut();
    var ans: u32 = 0;

    for (0..1000) |_| {
        var buffer: [1024]u8 = undefined;
        const line = nextLine(stdin.reader(), &buffer);
        var tmp: u32 = 0;
        var l: u32 = 10;
        var r: u32 = 10;
        for (0..line.len) |i| {
            const v = extract(line, i);

            if (v) |val| {
                if (l == 10) {
                    l = val;
                }
                r = val;
            }
        }
        tmp = l * 10 + r;
        try stdout.writer().print("{d}\n", .{tmp});
        ans += tmp;
    }
    try stdout.writer().print("{d}\n", .{ans});
}
