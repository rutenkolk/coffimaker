const std = @import("std");
const print = std.debug.print;
const os = std.os;
const c = @import("<<__ZIGCLJ_TRANSLATED_HEADER__>>");
const extreme_smoke_test = @import("./extreme_smoke_test.zig");

pub fn print_native_type_conversion(T: type) void {
    std.debug.print(" \"{s}\" \"{s}\"", .{
        @typeName(T),
        @typeName(@Type(@typeInfo(T))),
    });
}
pub fn print_all_native_type_conversions() void {
    std.debug.print("{{:type c-interop-to-native-type-conversion \n", .{});
    print_native_type_conversion(c_char);
    print_native_type_conversion(c_int);
    print_native_type_conversion(c_longdouble);
    print_native_type_conversion(c_longlong);
    print_native_type_conversion(c_short);
    print_native_type_conversion(c_uint);
    print_native_type_conversion(c_ulong);
    print_native_type_conversion(c_ulonglong);
    print_native_type_conversion(c_ushort);
    std.debug.print("}}\n", .{});
}

pub fn namespaced_declname(parent: type, decl: DeclInfo) []const u8 {
    return @typeName(parent) ++ "/" ++ decl.name;
}

pub fn main() void {
    @setEvalBranchQuota(9000);
    //std.time.sleep(1000000000);
    std.debug.print("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n", .{});
    //print_all_native_type_conversions();
    const run_extreme_smoke_test = false; //TODO: hook into build setup

    std.debug.print("[\n", .{});
    print_module_decls(if (run_extreme_smoke_test) extreme_smoke_test else c);
    std.debug.print("\n]\n", .{});
}

fn print_module_decls(T: type) void {
    inline for (allDecls(T)) |decl| {
        switch (decl.info) {
            .@"fn" => |info| {
                switch (std.mem.eql(u8, decl.name, "CLITERAL")) {
                    inline false => {
                        std.debug.print("{{:type :fn\n :name :{s}\n :params [", .{decl.name});
                        inline for (info.params, 0..) |param, i| {
                            const param_name = @field(T, "__zigclj_fn_param_names_" ++ decl.name)[i];
                            std.debug.print("[", .{});
                            print_custom_typename(param.type orelse anyerror);
                            std.debug.print(" :{s}]", .{param_name});
                        }
                        std.debug.print("]\n :return-type ", .{});
                        print_custom_typename(info.return_type orelse anyerror);
                        std.debug.print("}}\n", .{});
                    },
                    inline true => {},
                }
            },
            .type => |_| {
                const type_info = @typeInfo(@field(T, decl.name));
                std.debug.print("{{:type :type\n :name :{s}\n", .{namespaced_declname(T, decl)});
                switch (type_info) {
                    inline .@"struct", .@"enum", .@"union" => |container_info| {
                        const kind_str = switch (type_info) {
                            inline .@"struct" => ":struct",
                            inline .@"enum" => ":enum",
                            inline .@"union" => ":union",
                            else => unreachable,
                        };

                        std.debug.print(" :kind {s}\n :internal_reference :{s}\n :members [", .{ kind_str, internal_reference(@Type(type_info)) });
                        inline for (container_info.fields) |field| {
                            std.debug.print(" [", .{});
                            switch (type_info) {
                                else => unreachable,
                                .@"enum" => {}, //TODO: field.type doesn't exist for enum fields
                                inline .@"struct", .@"union" => print_custom_typename(field.type),
                            }
                            std.debug.print(" :{s}]", .{field.name});
                        }
                        std.debug.print(" ]}}\n", .{});
                    },
                    inline .@"opaque" => {
                        std.debug.print(" :kind :opaque}}\n", .{});
                    },
                    inline else => {
                        std.debug.print(" :kind :alias\n :alias-of ", .{});
                        print_custom_typename(@field(T, decl.name));
                        std.debug.print("}}\n", .{});
                    },
                }
            },
            else => {
                if (!std.mem.eql(u8, decl.name[0..1], "_")) {
                    const runtime_val = @field(T, decl.name);
                    std.debug.print("{{:type :constant\n :kind ", .{});
                    print_custom_typename(@TypeOf(runtime_val));
                    std.debug.print("\n :name :{s}\n :value ", .{decl.name});
                    print_value(runtime_val);
                    std.debug.print("}}\n", .{});
                }
            },
        }
    }
}

fn internal_reference(x: anytype) []const u8 {
    const prefix_len = "coffimaker.print_module_decls__anon_xxxxx__".len;
    return switch (@TypeOf(x)) {
        inline type => @typeName(@Type(@typeInfo(x)))[prefix_len..],
        inline std.builtin.Type => @typeName(@Type(x))[prefix_len..],
        inline else => unreachable,
    };
}

fn print_value(runtime_value: anytype) void {
    const T = @TypeOf(runtime_value);
    switch (@typeInfo(T)) {
        .@"struct" => |info| {
            std.debug.print("[", .{});
            inline for (info.fields) |field| {
                std.debug.print(" ", .{});
                print_value(@field(runtime_value, field.name));
            }
            std.debug.print(" ]", .{});
        },
        .array => {
            //const array_value: []info.child = runtime_value;
            std.debug.print("[", .{});
            inline for (runtime_value) |element| {
                std.debug.print(" ", .{});
                print_value(element);
            }
            std.debug.print(" ]", .{});
        },
        .float, .int => {
            std.debug.print("{d}", .{runtime_value});
        },
        .pointer => |info| {
            if (@intFromPtr(runtime_value) == 0) {
                std.debug.print(":nullptr", .{});
            } else if (info.child == u8) {
                std.debug.print("\"{s}\"", .{runtime_value});
            } else {
                switch (@typeInfo(info.child)) {
                    .array => |arrayinf| {
                        if (arrayinf.child == u8) {
                            std.debug.print("\"{s}\"", .{runtime_value.*});
                        } else {
                            std.debug.print("[:pointer ", .{});
                            print_value(runtime_value.*);
                            std.debug.print("]", .{});
                        }
                    },
                    else => {
                        std.debug.print("[:pointer ", .{});
                        print_value(runtime_value.*);
                        std.debug.print("]", .{});
                    },
                }
            }
        },
        else => {
            std.debug.print("{?}", .{runtime_value});
        },
    }
}

fn print_custom_typename(T: type) void {
    switch (@typeInfo(T)) {
        inline .pointer => |p| {
            switch (@typeInfo(p.child)) {
                .array => |array_info| {
                    if (array_info.child == u8) {
                        std.debug.print(":string", .{});
                    } else {
                        std.debug.print("[:pointer ", .{});
                        print_custom_typename(p.child);
                        std.debug.print("]", .{});
                    }
                },
                else => {
                    std.debug.print("[:pointer ", .{});
                    print_custom_typename(p.child);
                    std.debug.print("]", .{});
                },
            }
        },
        inline .array => |a| {
            std.debug.print("[:array ", .{});
            print_custom_typename(a.child);
            std.debug.print(" {d}]", .{a.len});
        },
        inline .optional => |o| {
            switch (@typeInfo(o.child)) {
                .pointer => |p| {
                    if (p.child == anyopaque) {
                        std.debug.print(":void-pointer", .{});
                    } else if (@typeInfo(p.child) == .@"fn") {
                        std.debug.print("[:function-pointer ", .{});
                        print_custom_typename(p.child);
                        std.debug.print("]", .{});
                    } else {
                        std.debug.print("[:pointer ", .{});
                        print_custom_typename(p.child);
                        std.debug.print("]", .{});
                    }
                },
                else => {
                    print_zig_native_typename(T);
                },
            }
        },
        inline .@"fn" => |f| {
            const return_type = f.return_type orelse void;
            std.debug.print("[ ", .{});
            inline for (f.params) |param| {
                const param_type = param.type orelse void;
                print_custom_typename(param_type);
                std.debug.print(" ", .{});
            }
            print_custom_typename(return_type);
            std.debug.print("]", .{});
        },
        inline else => {
            print_zig_native_typename(T);
        },
    }
}

fn print_zig_native_typename(T: type) void {
    const name = @typeName(T);
    if (name.len > 2 and std.mem.eql(u8, name[0..2], "c_")) {
        std.debug.print(":{s}", .{@typeName(@Type(@typeInfo(T)))});
    } else if (std.mem.containsAtLeast(u8, name, 1, ".")) {
        var it = std.mem.splitScalar(u8, name, '.');
        while (it.next() != null) {
            if (it.peek() != null) {
                const last_dot = it.index.? - 1;
                std.debug.print(":{s}/{s}", .{ name[0..last_dot], it.rest() });
            }
        }
    } else {
        std.debug.print(":{s}", .{name});
    }
}

const DeclInfo = struct {
    name: []const u8,
    info: std.builtin.Type,
};

fn allDecls(T: type) []const DeclInfo {
    var decllist: []const DeclInfo = &.{};
    const decls: []const std.builtin.Type.Declaration = switch (@typeInfo(T)) {
        inline .@"struct", .@"enum", .@"union", .@"opaque" => |container_info| container_info.decls,
        inline else => |_, tag| @compileError(@typeName(T) ++ " should be a container type (struct, union, enum, opaque), but was '" ++ @tagName(tag) ++ "'"),
    };
    for (decls) |decl| {
        if (!std.mem.eql(u8, decl.name[0..1], "_")) {
            const info = @typeInfo(@TypeOf(@field(T, decl.name)));
            switch (info) {
                .@"fn", .@"struct", .type => {
                    decllist = decllist ++ [_]DeclInfo{.{ .name = decl.name, .info = info }};
                },
                else => {
                    decllist = decllist ++ [_]DeclInfo{.{ .name = decl.name, .info = info }};
                },
            }
        }
    }
    return decllist;
}
