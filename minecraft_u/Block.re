exception Invalid_block;

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
  
  let stair_dir_parse: int => stair_dir = fun
  | 0 => E
  | 1 => W
  | 2 => S
  | 3 => N
  | 4 => Ed
  | 5 => Wd
  | 6 => Sd
  | 7 => Nd
  | _ => raise(Invalid_block);

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
  
let parse = (id, data) => switch (id, data) {
  | (0,_) =>  Air 
  | (1,_) =>  Stone 
  | (2,_) =>  Grass 
  | (3,_) =>  Dirt 
  | (4,_) =>  Cobblestone 
  | (5,_) =>  Planks 
  | (6,_) =>  Sapling 
  | (7,_) =>  Bedrock 
  | (8,level) =>  Flowing_water(level) 
  | (9,_) =>  Water 
  | (10,_) =>  Flowing_lava 
  | (11,_) =>  Lava 
  | (12,_) =>  Sand 
  | (13,_) =>  Gravel 
  | (14,_) =>  Gold_ore 
  | (15,_) =>  Iron_ore 
  | (16,_) =>  Coal_ore 
  | (17,_) =>  Log 
  | (18,_) =>  Leaves 
  | (19,_) =>  Sponge 
  | (20,_) =>  Glass 
  | (21,_) =>  Lapis_ore 
  | (22,_) =>  Lapis_block 
  | (23,_) =>  Dispenser 
  | (24,_) =>  Sandstone 
  | (25,_) =>  Noteblock 
  | (26,0) =>  Bed(S, Foot) 
  | (26,0x1) =>  Bed(W, Foot) 
  | (26,0x2) =>  Bed(N, Foot) 
  | (26,0x3) =>  Bed(E, Foot) 
  | (26,0x80) =>  Bed(S, Head) 
  | (26,0x81) =>  Bed(W, Head) 
  | (26,0x82) =>  Bed(N, Head) 
  | (26,0x83) =>  Bed(E, Head) 
  | (27,_) =>  Golden_rail 
  | (28,_) =>  Detector_rail 
  | (29,_) =>  Sticky_piston 
  | (30,_) =>  Web 
  | (31,_) =>  Tallgrass 
  | (32,_) =>  Deadbush 
  | (33,_) =>  Piston 
  | (34,_) =>  Piston_head 
  | (35,_) =>  Wool 
  | (36,_) =>  Piston_extension 
  | (37,_) =>  Yellow_flower 
  | (38,_) =>  Red_flower 
  | (39,_) =>  Brown_mushroom 
  | (40,_) =>  Red_mushroom 
  | (41,_) =>  Gold_block 
  | (42,_) =>  Iron_block 
  | (43,_) =>  Double_stone_slab 
  | (44,_) =>  Stone_slab 
  | (45,_) =>  Brick_block 
  | (46,_) =>  Tnt 
  | (47,_) =>  Bookshelf 
  | (48,_) =>  Mossy_cobblestone 
  | (49,_) =>  Obsidian 
  | (50,1) =>  Torch(E) 
  | (50,2) =>  Torch(W) 
  | (50,3) =>  Torch(S) 
  | (50,4) =>  Torch(N) 
  | (50,5) =>  Torch(Up) 
  | (51,_) =>  Fire 
  | (52,_) =>  Mob_spawner 
  | (53,d) =>  Oak_stairs(stair_dir_parse(d)) 
  | (54,_) =>  Chest 
  | (55,_) =>  Redstone_wire 
  | (56,_) =>  Diamond_ore 
  | (57,_) =>  Diamond_block 
  | (58,_) =>  Crafting_table 
  | (59,_) =>  Wheat 
  | (60,_) =>  Farmland 
  | (61,_) =>  Furnace 
  | (62,_) =>  Lit_furnace 
  | (63,_) =>  Standing_sign 
  | (64,_) =>  Wooden_door 
  | (65,_) =>  Ladder 
  | (66,_) =>  Rail 
  | (67,d) =>  Stone_stairs(stair_dir_parse(d)) 
  | (68,_) =>  Wall_sign 
  | (69,_) =>  Lever 
  | (70,_) =>  Stone_pressure_plate 
  | (71,_) =>  Iron_door 
  | (72,_) =>  Wooden_pressure_plate 
  | (73,_) =>  Redstone_ore 
  | (74,_) =>  Lit_redstone_ore 
  | (75,_) =>  Unlit_redstone_torch 
  | (76,_) =>  Redstone_torch 
  | (77,_) =>  Stone_button 
  | (78,_) =>  Snow_layer 
  | (79,_) =>  Ice 
  | (80,_) =>  Snow 
  | (81,_) =>  Cactus 
  | (82,_) =>  Clay 
  | (83,_) =>  Reeds 
  | (84,_) =>  Jukebox 
  | (85,_) =>  Fence 
  | (86,_) =>  Pumpkin 
  | (87,_) =>  Netherrack 
  | (88,_) =>  Soul_sand 
  | (89,_) =>  Glowstone 
  | (90,_) =>  Portal 
  | (91,_) =>  Lit_pumpkin 
  | (92,_) =>  Cake 
  | (93,_) =>  Unpowered_repeater 
  | (94,_) =>  Powered_repeater 
  | (95,_) =>  Stained_glass 
  | (96,_) =>  Trapdoor 
  | (97,_) =>  Monster_egg 
  | (98,_) =>  Stonebrick 
  | (99,_) =>  Brown_mushroom_block 
  | (100,_) =>  Red_mushroom_block 
  | (101,_) =>  Iron_bars 
  | (102,_) =>  Glass_pane 
  | (103,_) =>  Melon_block 
  | (104,_) =>  Pumpkin_stem 
  | (105,_) =>  Melon_stem 
  | (106,_) =>  Vine 
  | (107,_) =>  Fence_gate 
  | (108,d) =>  Brick_stairs(stair_dir_parse(d)) 
  | (109,d) =>  Stone_brick_stairs(stair_dir_parse(d)) 
  | (110,_) =>  Mycelium 
  | (111,_) =>  Waterlily 
  | (112,_) =>  Nether_brick 
  | (113,_) =>  Nether_brick_fence 
  | (114,d) =>  Nether_brick_stairs(stair_dir_parse(d)) 
  | (115,_) =>  Nether_wart 
  | (116,_) =>  Enchanting_table 
  | (117,_) =>  Brewing_stand 
  | (118,_) =>  Cauldron 
  | (119,_) =>  End_portal 
  | (120,_) =>  End_portal_frame 
  | (121,_) =>  End_stone 
  | (122,_) =>  Dragon_egg 
  | (123,_) =>  Redstone_lamp 
  | (124,_) =>  Lit_redstone_lamp 
  | (125,_) =>  Double_wooden_slab 
  | (126,_) =>  Wooden_slab 
  | (127,_) =>  Cocoa 
  | (128,d) =>  Sandstone_stairs(stair_dir_parse(d)) 
  | (129,_) =>  Emerald_ore 
  | (130,_) =>  Ender_chest 
  | (131,_) =>  Tripwire_hook 
  | (132,_) =>  Tripwire 
  | (133,_) =>  Emerald_block 
  | (134,d) =>  Spruce_stairs(stair_dir_parse(d)) 
  | (135,d) =>  Birch_stairs(stair_dir_parse(d)) 
  | (136,d) =>  Jungle_stairs(stair_dir_parse(d)) 
  | (137,_) =>  Command_block 
  | (138,_) =>  Beacon 
  | (139,_) =>  Cobblestone_wall 
  | (140,_) =>  Flower_pot 
  | (141,_) =>  Carrots 
  | (142,_) =>  Potatoes 
  | (143,_) =>  Wooden_button 
  | (144,_) =>  Skull 
  | (145,_) =>  Anvil 
  | (146,_) =>  Trapped_chest 
  | (147,_) =>  Light_weighted_pressure_plate 
  | (148,_) =>  Heavy_weighted_pressure_plate 
  | (149,_) =>  Unpowered_comparator 
  | (150,_) =>  Powered_comparator 
  | (151,_) =>  Daylight_detector 
  | (152,_) =>  Redstone_block 
  | (153,_) =>  Quartz_ore 
  | (154,_) =>  Hopper 
  | (155,_) =>  Quartz_block 
  | (156,d) =>  Quartz_stairs(stair_dir_parse(d)) 
  | (157,_) =>  Activator_rail 
  | (158,_) =>  Dropper 
  | (159,_) =>  Stained_hardened_clay 
  | (160,_) =>  Stained_glass_pane 
  | (161,_) =>  Leaves2 
  | (162,_) =>  Log2 
  | (163,d) =>  Acacia_stairs(stair_dir_parse(d)) 
  | (164,d) =>  Dark_oak_stairs(stair_dir_parse(d)) 
  | (165,_) =>  Slime_block 
  | (166,_) =>  Barrier 
  | (167,_) =>  Iron_trapdoor 
  | (168,_) =>  Prismarine 
  | (169,_) =>  Sea_lantern 
  | (170,_) =>  Hay_block 
  | (171,_) =>  Carpet 
  | (172,_) =>  Hardened_clay 
  | (173,_) =>  Coal_block 
  | (174,_) =>  Packed_ice 
  | (175,_) =>  Double_plant 
  | (176,_) =>  Standing_banner 
  | (177,_) =>  Wall_banner 
  | (178,_) =>  Daylight_detector_inverted 
  | (179,_) =>  Red_sandstone 
  | (180,d) =>  Red_sandstone_stairs(stair_dir_parse(d)) 
  | (181,_) =>  Double_stone_slab2 
  | (182,_) =>  Stone_slab2 
  | (183,_) =>  Spruce_fence_gate 
  | (184,_) =>  Birch_fence_gate 
  | (185,_) =>  Jungle_fence_gate 
  | (186,_) =>  Dark_oak_fence_gate 
  | (187,_) =>  Acacia_fence_gate 
  | (188,_) =>  Spruce_fence 
  | (189,_) =>  Birch_fence 
  | (190,_) =>  Jungle_fence 
  | (191,_) =>  Dark_oak_fence 
  | (192,_) =>  Acacia_fence 
  | (193,_) =>  Spruce_door 
  | (194,_) =>  Birch_door 
  | (195,_) =>  Jungle_door 
  | (196,_) =>  Acacia_door 
  | (197,_) =>  Dark_oak_door 
  | (_, _) => raise(Invalid_block)
};