const warn = @import("std").debug.warn;

const Node = struct {
    val: u128,
    left: ?*Node = null,
    right: ?*Node = null,
};

fn new_node(v: u128) Node {
    return Node{
        .val = v,
    };
}

pub fn main() void {
    var n = new_node(3);
    n.left = &new_node(4);

    warn("{}\n", .{n});

    // Will not enter at runtime because option is null
    if (n.right) |right| {
        warn("right: {}\n", .{right});
    }

    // Safe because it's a compile time error
    // warn("{}\n", .{n.right.?});

    // Prints out val
    warn("left: {}\n", .{n.left.?});
}
