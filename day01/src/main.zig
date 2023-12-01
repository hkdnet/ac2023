const std = @import("std");

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
        for (line) |c| {
            if (c >= '0' and c <= '9') {
                if (l == 10) {
                    l = c - '0';
                }
                r = c - '0';
            }
        }
        tmp = l * 10 + r;
        try stdout.writer().print("{d}\n", .{tmp});
        ans += tmp;
    }
    try stdout.writer().print("{d}\n", .{ans});
}
