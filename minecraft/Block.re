type stair_dir =
  | E
  | W
  | S
  | N
  | Ed
  | Wd
  | Sd
  | Nd;

type torch_dir =
  | E
  | W
  | S
  | N
  | Up;

type bed_dir =
  | S
  | W
  | N
  | E;

type bed_part =
  | Foot
  | Head;

/* Materials list taken from https://github.com/MorbZ/J2Blocks/blob/master/src/net/morbz/minecraft/blocks/Material.java */
type material =
  | Air
  | Stone
  | Grass
  | Dirt
  | Cobblestone
  | Planks
  | Sapling
  | Bedrock
  | Flowing_water(int)
  | Water
  | Flowing_lava
  | Lava
  | Sand
  | Gravel
  | Gold_ore
  | Iron_ore
  | Coal_ore
  | Log
  | Leaves
  | Sponge
  | Glass
  | Lapis_ore
  | Lapis_block
  | Dispenser
  | Sandstone
  | Noteblock
  | Bed(bed_dir, bed_part)
  | Golden_rail
  | Detector_rail
  | Sticky_piston
  | Web
  | Tallgrass
  | Deadbush
  | Piston
  | Piston_head
  | Wool
  | Piston_extension
  | Yellow_flower
  | Red_flower
  | Brown_mushroom
  | Red_mushroom
  | Gold_block
  | Iron_block
  | Double_stone_slab
  | Stone_slab
  | Brick_block
  | Tnt
  | Bookshelf
  | Mossy_cobblestone
  | Obsidian
  | Torch(torch_dir)
  | Fire
  | Mob_spawner
  | Oak_stairs(stair_dir)
  | Chest
  | Redstone_wire
  | Diamond_ore
  | Diamond_block
  | Crafting_table
  | Wheat
  | Farmland
  | Furnace
  | Lit_furnace
  | Standing_sign
  | Wooden_door
  | Ladder
  | Rail
  | Stone_stairs(stair_dir)
  | Wall_sign
  | Lever
  | Stone_pressure_plate
  | Iron_door
  | Wooden_pressure_plate
  | Redstone_ore
  | Lit_redstone_ore
  | Unlit_redstone_torch
  | Redstone_torch
  | Stone_button
  | Snow_layer
  | Ice
  | Snow
  | Cactus
  | Clay
  | Reeds
  | Jukebox
  | Fence
  | Pumpkin
  | Netherrack
  | Soul_sand
  | Glowstone
  | Portal
  | Lit_pumpkin
  | Cake
  | Unpowered_repeater
  | Powered_repeater
  | Stained_glass
  | Trapdoor
  | Monster_egg
  | Stonebrick
  | Brown_mushroom_block
  | Red_mushroom_block
  | Iron_bars
  | Glass_pane
  | Melon_block
  | Pumpkin_stem
  | Melon_stem
  | Vine
  | Fence_gate
  | Brick_stairs(stair_dir)
  | Stone_brick_stairs(stair_dir)
  | Mycelium
  | Waterlily
  | Nether_brick
  | Nether_brick_fence
  | Nether_brick_stairs(stair_dir)
  | Nether_wart
  | Enchanting_table
  | Brewing_stand
  | Cauldron
  | End_portal
  | End_portal_frame
  | End_stone
  | Dragon_egg
  | Redstone_lamp
  | Lit_redstone_lamp
  | Double_wooden_slab
  | Wooden_slab
  | Cocoa
  | Sandstone_stairs(stair_dir)
  | Emerald_ore
  | Ender_chest
  | Tripwire_hook
  | Tripwire
  | Emerald_block
  | Spruce_stairs(stair_dir)
  | Birch_stairs(stair_dir)
  | Jungle_stairs(stair_dir)
  | Command_block
  | Beacon
  | Cobblestone_wall
  | Flower_pot
  | Carrots
  | Potatoes
  | Wooden_button
  | Skull
  | Anvil
  | Trapped_chest
  | Light_weighted_pressure_plate
  | Heavy_weighted_pressure_plate
  | Unpowered_comparator
  | Powered_comparator
  | Daylight_detector
  | Redstone_block
  | Quartz_ore
  | Hopper
  | Quartz_block
  | Quartz_stairs(stair_dir)
  | Activator_rail
  | Dropper
  | Stained_hardened_clay
  | Stained_glass_pane
  | Leaves2
  | Log2
  | Acacia_stairs(stair_dir)
  | Dark_oak_stairs(stair_dir)
  | Slime_block
  | Barrier
  | Iron_trapdoor
  | Prismarine
  | Sea_lantern
  | Hay_block
  | Carpet
  | Hardened_clay
  | Coal_block
  | Packed_ice
  | Double_plant
  | Standing_banner
  | Wall_banner
  | Daylight_detector_inverted
  | Red_sandstone
  | Red_sandstone_stairs(stair_dir)
  | Double_stone_slab2
  | Stone_slab2
  | Spruce_fence_gate
  | Birch_fence_gate
  | Jungle_fence_gate
  | Dark_oak_fence_gate
  | Acacia_fence_gate
  | Spruce_fence
  | Birch_fence
  | Jungle_fence
  | Dark_oak_fence
  | Acacia_fence
  | Spruce_door
  | Birch_door
  | Jungle_door
  | Acacia_door
  | Dark_oak_door;

let id =
  fun
  | Air => 0
  | Stone => 1
  | Grass => 2
  | Dirt => 3
  | Cobblestone => 4
  | Planks => 5
  | Sapling => 6
  | Bedrock => 7
  | Flowing_water(_) => 8
  | Water => 9
  | Flowing_lava => 10
  | Lava => 11
  | Sand => 12
  | Gravel => 13
  | Gold_ore => 14
  | Iron_ore => 15
  | Coal_ore => 16
  | Log => 17
  | Leaves => 18
  | Sponge => 19
  | Glass => 20
  | Lapis_ore => 21
  | Lapis_block => 22
  | Dispenser => 23
  | Sandstone => 24
  | Noteblock => 25
  | Bed(_, _) => 26
  | Golden_rail => 27
  | Detector_rail => 28
  | Sticky_piston => 29
  | Web => 30
  | Tallgrass => 31
  | Deadbush => 32
  | Piston => 33
  | Piston_head => 34
  | Wool => 35
  | Piston_extension => 36
  | Yellow_flower => 37
  | Red_flower => 38
  | Brown_mushroom => 39
  | Red_mushroom => 40
  | Gold_block => 41
  | Iron_block => 42
  | Double_stone_slab => 43
  | Stone_slab => 44
  | Brick_block => 45
  | Tnt => 46
  | Bookshelf => 47
  | Mossy_cobblestone => 48
  | Obsidian => 49
  | Torch(_) => 50
  | Fire => 51
  | Mob_spawner => 52
  | Oak_stairs(_) => 53
  | Chest => 54
  | Redstone_wire => 55
  | Diamond_ore => 56
  | Diamond_block => 57
  | Crafting_table => 58
  | Wheat => 59
  | Farmland => 60
  | Furnace => 61
  | Lit_furnace => 62
  | Standing_sign => 63
  | Wooden_door => 64
  | Ladder => 65
  | Rail => 66
  | Stone_stairs(_) => 67
  | Wall_sign => 68
  | Lever => 69
  | Stone_pressure_plate => 70
  | Iron_door => 71
  | Wooden_pressure_plate => 72
  | Redstone_ore => 73
  | Lit_redstone_ore => 74
  | Unlit_redstone_torch => 75
  | Redstone_torch => 76
  | Stone_button => 77
  | Snow_layer => 78
  | Ice => 79
  | Snow => 80
  | Cactus => 81
  | Clay => 82
  | Reeds => 83
  | Jukebox => 84
  | Fence => 85
  | Pumpkin => 86
  | Netherrack => 87
  | Soul_sand => 88
  | Glowstone => 89
  | Portal => 90
  | Lit_pumpkin => 91
  | Cake => 92
  | Unpowered_repeater => 93
  | Powered_repeater => 94
  | Stained_glass => 95
  | Trapdoor => 96
  | Monster_egg => 97
  | Stonebrick => 98
  | Brown_mushroom_block => 99
  | Red_mushroom_block => 100
  | Iron_bars => 101
  | Glass_pane => 102
  | Melon_block => 103
  | Pumpkin_stem => 104
  | Melon_stem => 105
  | Vine => 106
  | Fence_gate => 107
  | Brick_stairs(_) => 108
  | Stone_brick_stairs(_) => 109
  | Mycelium => 110
  | Waterlily => 111
  | Nether_brick => 112
  | Nether_brick_fence => 113
  | Nether_brick_stairs(_) => 114
  | Nether_wart => 115
  | Enchanting_table => 116
  | Brewing_stand => 117
  | Cauldron => 118
  | End_portal => 119
  | End_portal_frame => 120
  | End_stone => 121
  | Dragon_egg => 122
  | Redstone_lamp => 123
  | Lit_redstone_lamp => 124
  | Double_wooden_slab => 125
  | Wooden_slab => 126
  | Cocoa => 127
  | Sandstone_stairs(_) => 128
  | Emerald_ore => 129
  | Ender_chest => 130
  | Tripwire_hook => 131
  | Tripwire => 132
  | Emerald_block => 133
  | Spruce_stairs(_) => 134
  | Birch_stairs(_) => 135
  | Jungle_stairs(_) => 136
  | Command_block => 137
  | Beacon => 138
  | Cobblestone_wall => 139
  | Flower_pot => 140
  | Carrots => 141
  | Potatoes => 142
  | Wooden_button => 143
  | Skull => 144
  | Anvil => 145
  | Trapped_chest => 146
  | Light_weighted_pressure_plate => 147
  | Heavy_weighted_pressure_plate => 148
  | Unpowered_comparator => 149
  | Powered_comparator => 150
  | Daylight_detector => 151
  | Redstone_block => 152
  | Quartz_ore => 153
  | Hopper => 154
  | Quartz_block => 155
  | Quartz_stairs(_) => 156
  | Activator_rail => 157
  | Dropper => 158
  | Stained_hardened_clay => 159
  | Stained_glass_pane => 160
  | Leaves2 => 161
  | Log2 => 162
  | Acacia_stairs(_) => 163
  | Dark_oak_stairs(_) => 164
  | Slime_block => 165
  | Barrier => 166
  | Iron_trapdoor => 167
  | Prismarine => 168
  | Sea_lantern => 169
  | Hay_block => 170
  | Carpet => 171
  | Hardened_clay => 172
  | Coal_block => 173
  | Packed_ice => 174
  | Double_plant => 175
  | Standing_banner => 176
  | Wall_banner => 177
  | Daylight_detector_inverted => 178
  | Red_sandstone => 179
  | Red_sandstone_stairs(_) => 180
  | Double_stone_slab2 => 181
  | Stone_slab2 => 182
  | Spruce_fence_gate => 183
  | Birch_fence_gate => 184
  | Jungle_fence_gate => 185
  | Dark_oak_fence_gate => 186
  | Acacia_fence_gate => 187
  | Spruce_fence => 188
  | Birch_fence => 189
  | Jungle_fence => 190
  | Dark_oak_fence => 191
  | Acacia_fence => 192
  | Spruce_door => 193
  | Birch_door => 194
  | Jungle_door => 195
  | Acacia_door => 196
  | Dark_oak_door => 197;

let stair_dir_data: stair_dir => int =
  fun
  | E => 0
  | W => 1
  | S => 2
  | N => 3
  | Ed => 4
  | Wd => 5
  | Sd => 6
  | Nd => 7;

let torch_dir_data: torch_dir => int =
  fun
  | E => 1
  | W => 2
  | S => 3
  | N => 4
  | Up => 5;

let bed_data = (dir: bed_dir, part: bed_part) => {
  let lower =
    switch (dir) {
    | S => 0
    | W => 1
    | N => 2
    | E => 3
    };
  let upper =
    switch (part) {
    | Foot => 0
    | Head => 0x8
    };
  lower lor upper;
};

let data =
  fun
  | Flowing_water(level) => level
  | Sapling => 0x8
  | Tallgrass => 1
  | Torch(dir) => torch_dir_data(dir)
  | Bed(dir, part) => bed_data(dir, part)
  /* Stairs */
  | Oak_stairs(dir)
  | Stone_stairs(dir)
  | Brick_stairs(dir)
  | Stone_brick_stairs(dir)
  | Nether_brick_stairs(dir)
  | Sandstone_stairs(dir)
  | Spruce_stairs(dir)
  | Birch_stairs(dir)
  | Jungle_stairs(dir)
  | Quartz_stairs(dir)
  | Acacia_stairs(dir)
  | Dark_oak_stairs(dir)
  | Red_sandstone_stairs(dir) => stair_dir_data(dir)
  /* Other/unimplemented */
  | _ => 0;