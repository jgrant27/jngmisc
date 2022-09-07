const printf = @import("std").debug.print;

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

    printf("{}\n", .{n});

    // Will not enter at runtime because option is null
    if (n.right) |right| {
        printf("right: {}\n", .{right});
    }

    // Safe because it's a compile time error
    // printf("{}\n", .{n.right.?});

    // Prints out val
    printf("left: {}\n", .{n.left.?});
}
