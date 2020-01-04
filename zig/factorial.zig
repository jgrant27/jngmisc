const warn = @import("std").debug.warn;

//@Type(comptime info: @import("builtin").TypeInfo) type

//fn factorial(n: var) @TypeOf(n) {
fn factorial(n: u128) u128 {
    if (n < 2) return 1 else return n * factorial(n - 1);
}

pub fn main() void {
    // // Allocate an integer on the heap
    // var heap_allocator = @import("std").heap.c_allocator;
    // var a = try builtin.BigInt.init(heap_allocator);
    // defer a.deinit();
    warn("{}\n", .{factorial(1)}); // 1
    warn("{}\n", .{factorial(5)}); // 120
    warn("{}\n", .{factorial(7)}); // 5040
    warn("{}\n", .{factorial(34)}); // 295232799039604140847618609643520000000
    //warn("{}\n", .{factorial(35)}); //
}
