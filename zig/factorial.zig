const printf = @import("std").debug.print;

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
    printf("{}\n", .{factorial(1)}); // 1
    printf("{}\n", .{factorial(5)}); // 120
    printf("{}\n", .{factorial(7)}); // 5040
    printf("{}\n", .{factorial(34)}); // 295232799039604140847618609643520000000
    //printf("{}\n", .{factorial(35)}); //
}
