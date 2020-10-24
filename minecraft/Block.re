exception Invalid_block(string);

type stair_dir =
  | E
  | W
  | S
  | N
  | Ed
  | Wd
  | Sd
  | Nd;

type dir =
  | N
  | E
  | S
  | W;

type bed_part =
  | Foot
  | Head;

type door_part =
  | Lower
  | Upper;

type slab_type =
  | Bottom
  | Top
  | Double;

type axis =
  | X
  | Y
  | Z;

/* Materials list taken from https://minecraft.gamepedia.com/Java_Edition_data_value */
type material =
  /* Air needs to be 0 */
  | Air
  /* Fluids */
  | Flowing_water(int)
  /* Blocks */
  | Acacia_button
  | Acacia_door
  | Acacia_fence_gate
  | Acacia_fence
  | Acacia_leaves
  | Acacia_log
  | Acacia_planks
  | Acacia_pressure_plate
  | Acacia_sapling
  | Acacia_sign
  | Acacia_slab
  | Acacia_stairs
  | Acacia_trapdoor
  | Acacia_wall_sign
  | Acacia_wood
  | Activator_rail
  | Allium
  | Ancient_debris
  | Andesite
  | Andesite_slab
  | Andesite_stairs
  | Andesite_wall
  | Anvil
  | Attached_melon_stem
  | Attached_pumpkin_stem
  | Azure_bluet
  | Bamboo
  | Bamboo_sapling
  | Barrel
  | Barrier
  | Basalt
  | Beacon
  | Bedrock
  | Beehive
  | Bee_nest
  | Beetroots
  | Bell
  | Birch_button
  | Birch_door
  | Birch_fence_gate
  | Birch_fence
  | Birch_leaves
  | Birch_log
  | Birch_planks
  | Birch_pressure_plate
  | Birch_sapling
  | Birch_sign
  | Birch_slab
  | Birch_stairs
  | Birch_trapdoor
  | Birch_wall_sign
  | Birch_wood
  | Black_banner
  | Black_bed
  | Black_carpet
  | Black_concrete_powder
  | Black_concrete
  | Black_glazed_terracotta
  | Black_shulker_box
  | Black_stained_glass
  | Black_stained_glass_pane
  | Black_terracotta
  | Black_wall_banner
  | Black_wool
  | Blackstone
  | Blackstone_slab
  | Blackstone_stairs
  | Blackstone_wall
  | Blast_furnace
  | Blue_banner
  | Blue_bed
  | Blue_carpet
  | Blue_concrete_powder
  | Blue_concrete
  | Blue_glazed_terracotta
  | Blue_ice
  | Blue_orchid
  | Blue_shulker_box
  | Blue_stained_glass
  | Blue_stained_glass_pane
  | Blue_terracotta
  | Blue_wall_banner
  | Blue_wool
  | Bone_block
  | Bookshelf
  | Brain_coral
  | Brain_coral_block
  | Brain_coral_fan
  | Brain_coral_wall_fan
  | Brewing_stand
  | Brick_slab
  | Brick_stairs
  | Brick_wall
  | Bricks
  | Brown_banner
  | Brown_bed
  | Brown_carpet
  | Brown_concrete_powder
  | Brown_concrete
  | Brown_glazed_terracotta
  | Brown_mushroom_block
  | Brown_mushroom
  | Brown_shulker_box
  | Brown_stained_glass
  | Brown_stained_glass_pane
  | Brown_terracotta
  | Brown_wall_banner
  | Brown_wool
  | Bubble_column
  | Bubble_coral
  | Bubble_coral_block
  | Bubble_coral_fan
  | Bubble_coral_wall_fan
  | Cactus
  | Chain
  | Cake
  | Campfire
  | Carrots
  | Cartography_table
  | Carved_pumpkin
  | Cauldron
  | Cave_air
  | Chain_command_block
  | Chest
  | Chipped_anvil
  | Chiseled_nether_bricks
  | Chiseled_polished_blackstone
  | Chiseled_quartz_block
  | Chiseled_red_sandstone
  | Chiseled_sandstone
  | Chiseled_stone_bricks
  | Chorus_flower
  | Chorus_plant
  | Clay
  | Coal_block
  | Coal_ore
  | Coarse_dirt
  | Cobblestone
  | Cobblestone_slab
  | Cobblestone_stairs(stair_dir)
  | Cobblestone_wall
  | Cobweb
  | Cocoa
  | Command_block
  | Comparator
  | Composter
  | Conduit
  | Cornflower
  | Cracked_nether_bricks
  | Cracked_polished_blackstone_bricks
  | Cracked_stone_bricks
  | Crafting_table
  | Creeper_head
  | Creeper_wall_head
  | Crimson_button
  | Crimson_door
  | Crimson_fence_gate
  | Crimson_fence
  | Crimson_fungus
  | Crimson_hyphae
  | Crimson_nylium
  | Crimson_planks
  | Crimson_pressure_plate
  | Crimson_roots
  | Crimson_sign
  | Crimson_slab
  | Crimson_stairs
  | Crimson_stem
  | Crimson_trapdoor
  | Crimson_wall_sign
  | Crying_obsidian
  | Cut_red_sandstone
  | Cut_red_sandstone_slab
  | Cut_sandstone
  | Cut_sandstone_slab
  | Cyan_banner
  | Cyan_bed
  | Cyan_carpet
  | Cyan_concrete_powder
  | Cyan_concrete
  | Cyan_glazed_terracotta
  | Cyan_shulker_box
  | Cyan_stained_glass
  | Cyan_stained_glass_pane
  | Cyan_terracotta
  | Cyan_wall_banner
  | Cyan_wool
  | Damaged_anvil
  | Dandelion
  | Dark_oak_button
  | Dark_oak_door
  | Dark_oak_fence_gate
  | Dark_oak_fence
  | Dark_oak_leaves
  | Dark_oak_log
  | Dark_oak_planks
  | Dark_oak_pressure_plate
  | Dark_oak_sapling
  | Dark_oak_sign
  | Dark_oak_slab
  | Dark_oak_stairs
  | Dark_oak_trapdoor
  | Dark_oak_wall_sign
  | Dark_oak_wood
  | Dark_prismarine
  | Dark_prismarine_slab
  | Dark_prismarine_stairs
  | Daylight_detector
  | Dead_brain_coral
  | Dead_brain_coral_block
  | Dead_brain_coral_fan
  | Dead_brain_coral_wall_fan
  | Dead_bubble_coral
  | Dead_bubble_coral_block
  | Dead_bubble_coral_fan
  | Dead_bubble_coral_wall_fan
  | Dead_bush
  | Dead_fire_coral
  | Dead_fire_coral_block
  | Dead_fire_coral_fan
  | Dead_fire_coral_wall_fan
  | Dead_horn_coral
  | Dead_horn_coral_block
  | Dead_horn_coral_fan
  | Dead_horn_coral_wall_fan
  | Dead_tube_coral
  | Dead_tube_coral_block
  | Dead_tube_coral_fan
  | Dead_tube_coral_wall_fan
  | Detector_rail
  | Diamond_block
  | Diamond_ore
  | Diorite
  | Diorite_slab
  | Diorite_stairs
  | Diorite_wall
  | Dirt
  | Dispenser
  | Dragon_egg
  | Dragon_head
  | Dragon_wall_head
  | Dried_kelp_block
  | Dropper
  | Emerald_block
  | Emerald_ore
  | Enchanting_table
  | End_gateway
  | End_portal_frame
  | End_portal
  | End_rod
  | End_stone
  | End_stone_brick_slab
  | End_stone_brick_stairs
  | End_stone_brick_wall
  | End_stone_bricks
  | Ender_chest
  | Farmland
  | Fern
  | Fire
  | Fire_coral
  | Fire_coral_block
  | Fire_coral_fan
  | Fire_coral_wall_fan
  | Fletching_table
  | Flower_pot
  | Frosted_ice
  | Furnace
  | Gilded_blackstone
  | Glass
  | Glass_pane
  | Glowstone
  | Gold_block
  | Gold_ore
  | Granite
  | Granite_slab
  | Granite_stairs
  | Granite_wall
  | Grass_block
  | Grass_path
  | Grass
  | Gravel
  | Gray_banner
  | Gray_bed
  | Gray_carpet
  | Gray_concrete_powder
  | Gray_concrete
  | Gray_glazed_terracotta
  | Gray_shulker_box
  | Gray_stained_glass
  | Gray_stained_glass_pane
  | Gray_terracotta
  | Gray_wall_banner
  | Gray_wool
  | Green_banner
  | Green_bed
  | Green_carpet
  | Green_concrete_powder
  | Green_concrete
  | Green_glazed_terracotta
  | Green_shulker_box
  | Green_stained_glass
  | Green_stained_glass_pane
  | Green_terracotta
  | Green_wall_banner
  | Green_wool
  | Grindstone
  | Hay_block
  | Heavy_weighted_pressure_plate
  | Hopper
  | Honey_block
  | Honeycomb_block
  | Horn_coral
  | Horn_coral_block
  | Horn_coral_fan
  | Horn_coral_wall_fan
  | Ice
  | Infested_chiseled_stone_bricks
  | Infested_cobblestone
  | Infested_cracked_stone_bricks
  | Infested_mossy_stone_bricks
  | Infested_stone
  | Infested_stone_bricks
  | Iron_bars
  | Iron_door
  | Iron_block
  | Iron_ore
  | Iron_trapdoor
  | Jack_o_lantern
  | Jigsaw
  | Jukebox
  | Jungle_button
  | Jungle_door
  | Jungle_fence_gate
  | Jungle_fence
  | Jungle_leaves
  | Jungle_log
  | Jungle_planks
  | Jungle_pressure_plate
  | Jungle_sapling
  | Jungle_sign
  | Jungle_slab
  | Jungle_stairs
  | Jungle_trapdoor
  | Jungle_wall_sign
  | Jungle_wood
  | Kelp
  | Kelp_plant
  | Ladder
  | Lantern
  | Lapis_block
  | Lapis_ore
  | Large_fern
  | Lava
  | Lectern
  | Lever
  | Light_blue_banner
  | Light_blue_bed
  | Light_blue_carpet
  | Light_blue_concrete_powder
  | Light_blue_concrete
  | Light_blue_glazed_terracotta
  | Light_blue_shulker_box
  | Light_blue_stained_glass
  | Light_blue_stained_glass_pane
  | Light_blue_terracotta
  | Light_blue_wall_banner
  | Light_blue_wool
  | Light_gray_banner
  | Light_gray_bed
  | Light_gray_carpet
  | Light_gray_concrete_powder
  | Light_gray_concrete
  | Light_gray_glazed_terracotta
  | Light_gray_shulker_box
  | Light_gray_stained_glass
  | Light_gray_stained_glass_pane
  | Light_gray_terracotta
  | Light_gray_wall_banner
  | Light_gray_wool
  | Light_weighted_pressure_plate
  | Lilac
  | Lily_pad
  | Lily_of_the_valley
  | Lime_banner
  | Lime_bed
  | Lime_carpet
  | Lime_concrete_powder
  | Lime_concrete
  | Lime_glazed_terracotta
  | Lime_shulker_box
  | Lime_stained_glass
  | Lime_stained_glass_pane
  | Lime_terracotta
  | Lime_wall_banner
  | Lime_wool
  | Lodestone
  | Loom
  | Magenta_banner
  | Magenta_bed
  | Magenta_carpet
  | Magenta_concrete_powder
  | Magenta_concrete
  | Magenta_glazed_terracotta
  | Magenta_shulker_box
  | Magenta_stained_glass
  | Magenta_stained_glass_pane
  | Magenta_terracotta
  | Magenta_wall_banner
  | Magenta_wool
  | Magma_block
  | Melon
  | Melon_stem
  | Mossy_cobblestone
  | Mossy_cobblestone_slab
  | Mossy_cobblestone_stairs
  | Mossy_cobblestone_wall
  | Mossy_stone_brick_slab
  | Mossy_stone_brick_stairs
  | Mossy_stone_brick_wall
  | Mossy_stone_bricks
  | Moving_piston
  | Mushroom_stem
  | Mycelium
  | Nether_brick_fence
  | Nether_brick_slab
  | Nether_brick_stairs
  | Nether_brick_wall
  | Nether_bricks
  | Nether_gold_ore
  | Nether_portal
  | Nether_quartz_ore
  | Nether_sprouts
  | Nether_wart_block
  | Nether_wart
  | Netherite_block
  | Netherrack
  | Note_block
  | Oak_button
  | Oak_door(dir, door_part)
  | Oak_fence_gate
  | Oak_fence
  | Oak_leaves
  | Oak_log(axis)
  | Oak_planks
  | Oak_pressure_plate
  | Oak_sapling
  | Oak_sign
  | Oak_slab
  | Oak_stairs
  | Oak_trapdoor
  | Oak_wall_sign
  | Oak_wood
  | Observer
  | Obsidian
  | Orange_banner
  | Orange_bed(dir, bed_part)
  | Orange_carpet
  | Orange_concrete_powder
  | Orange_concrete
  | Orange_glazed_terracotta
  | Orange_shulker_box
  | Orange_stained_glass
  | Orange_stained_glass_pane
  | Orange_terracotta
  | Orange_tulip
  | Orange_wall_banner
  | Orange_wool
  | Oxeye_daisy
  | Packed_ice
  | Peony
  | Petrified_oak_slab
  | Pink_banner
  | Pink_bed
  | Pink_carpet
  | Pink_concrete_powder
  | Pink_concrete
  | Pink_glazed_terracotta
  | Pink_shulker_box
  | Pink_stained_glass
  | Pink_stained_glass_pane
  | Pink_terracotta
  | Pink_tulip
  | Pink_wall_banner
  | Pink_wool
  | Piston_head
  | Piston
  | Player_head
  | Player_wall_head
  | Podzol
  | Polished_andesite
  | Polished_andesite_slab
  | Polished_andesite_stairs
  | Polished_basalt
  | Polished_blackstone
  | Polished_blackstone_brick_slab
  | Polished_blackstone_brick_stairs
  | Polished_blackstone_brick_wall
  | Polished_blackstone_bricks
  | Polished_blackstone_button
  | Polished_blackstone_pressure_plate
  | Polished_blackstone_slab
  | Polished_blackstone_stairs
  | Polished_blackstone_wall
  | Polished_diorite
  | Polished_diorite_slab
  | Polished_diorite_stairs
  | Polished_granite
  | Polished_granite_slab
  | Polished_granite_stairs
  | Poppy
  | Potatoes
  | Potted_acacia_sapling
  | Potted_allium
  | Potted_azure_bluet
  | Potted_bamboo
  | Potted_birch_sapling
  | Potted_blue_orchid
  | Potted_brown_mushroom
  | Potted_cactus
  | Potted_cornflower
  | Potted_crimson_fungus
  | Potted_crimson_roots
  | Potted_dandelion
  | Potted_dark_oak_sapling
  | Potted_dead_bush
  | Potted_fern
  | Potted_jungle_sapling
  | Potted_lily_of_the_valley
  | Potted_oak_sapling
  | Potted_orange_tulip
  | Potted_oxeye_daisy
  | Potted_pink_tulip
  | Potted_poppy
  | Potted_red_mushroom
  | Potted_red_tulip
  | Potted_spruce_sapling
  | Potted_warped_fungus
  | Potted_warped_roots
  | Potted_white_tulip
  | Potted_wither_rose
  | Powered_rail
  | Prismarine
  | Prismarine_brick_slab
  | Prismarine_brick_stairs
  | Prismarine_bricks
  | Prismarine_slab
  | Prismarine_stairs
  | Prismarine_wall
  | Pumpkin
  | Pumpkin_stem
  | Purple_banner
  | Purple_bed
  | Purple_carpet
  | Purple_concrete_powder
  | Purple_concrete
  | Purple_glazed_terracotta
  | Purple_shulker_box
  | Purple_stained_glass
  | Purple_stained_glass_pane
  | Purple_terracotta
  | Purple_wall_banner
  | Purple_wool
  | Purpur_block
  | Purpur_pillar
  | Purpur_slab
  | Purpur_stairs
  | Quartz_block
  | Quartz_bricks
  | Quartz_pillar
  | Quartz_slab
  | Quartz_stairs
  | Rail
  | Red_banner
  | Red_bed
  | Red_carpet
  | Red_concrete_powder
  | Red_concrete
  | Red_glazed_terracotta
  | Red_mushroom_block
  | Red_mushroom
  | Red_nether_brick_slab
  | Red_nether_brick_stairs
  | Red_nether_brick_wall
  | Red_nether_bricks
  | Red_sand
  | Red_sandstone
  | Red_sandstone_slab
  | Red_sandstone_stairs
  | Red_sandstone_wall
  | Red_shulker_box
  | Red_stained_glass
  | Red_stained_glass_pane
  | Red_terracotta
  | Red_tulip
  | Red_wall_banner
  | Red_wool
  | Respawn_anchor
  | Redstone_block
  | Redstone_lamp
  | Redstone_ore
  | Redstone_torch
  | Redstone_wall_torch
  | Redstone_wire
  | Repeater
  | Repeating_command_block
  | Rose_bush
  | Sand
  | Sandstone
  | Sandstone_slab
  | Sandstone_stairs
  | Sandstone_wall
  | Scaffolding
  | Sea_lantern
  | Sea_pickle
  | Seagrass
  | Shroomlight
  | Shulker_box
  | Skeleton_skull
  | Skeleton_wall_skull
  | Slime_block
  | Smithing_table
  | Smoker
  | Smooth_quartz
  | Smooth_quartz_slab
  | Smooth_quartz_stairs
  | Smooth_red_sandstone
  | Smooth_red_sandstone_slab
  | Smooth_red_sandstone_stairs
  | Smooth_sandstone
  | Smooth_sandstone_slab
  | Smooth_sandstone_stairs
  | Smooth_stone
  | Smooth_stone_slab(slab_type)
  | Snow_block
  | Snow
  | Soul_campfire
  | Soul_fire
  | Soul_lantern
  | Soul_sand
  | Soul_soil
  | Soul_torch
  | Soul_wall_torch
  | Spawner
  | Sponge
  | Spruce_button
  | Spruce_door
  | Spruce_fence_gate
  | Spruce_fence
  | Spruce_leaves
  | Spruce_log
  | Spruce_planks
  | Spruce_pressure_plate
  | Spruce_sapling
  | Spruce_sign
  | Spruce_slab
  | Spruce_stairs
  | Spruce_trapdoor
  | Spruce_wall_sign
  | Spruce_wood
  | Sticky_piston
  | Stone
  | Stone_brick_slab
  | Stone_brick_stairs(stair_dir)
  | Stone_brick_wall
  | Stone_bricks
  | Stone_button
  | Stone_pressure_plate
  | Stone_slab
  | Stone_stairs(stair_dir)
  | Stonecutter
  | Stripped_acacia_log
  | Stripped_acacia_wood
  | Stripped_birch_log
  | Stripped_birch_wood
  | Stripped_crimson_hyphae
  | Stripped_crimson_stem
  | Stripped_dark_oak_log
  | Stripped_dark_oak_wood
  | Stripped_jungle_log
  | Stripped_jungle_wood
  | Stripped_oak_log
  | Stripped_oak_wood
  | Stripped_spruce_log
  | Stripped_spruce_wood
  | Stripped_warped_hyphae
  | Stripped_warped_stem
  | Structure_block
  | Structure_void
  | Sugar_cane
  | Sunflower
  | Sweet_berry_bush
  | Tall_grass
  | Tall_seagrass
  | Target
  | Terracotta
  | Tnt
  | Torch
  | Trapped_chest
  | Tripwire_hook
  | Tripwire
  | Tube_coral
  | Tube_coral_block
  | Tube_coral_fan
  | Tube_coral_wall_fan
  | Turtle_egg
  | Twisting_vines
  | Twisting_vines_plant
  | Vine
  | Void_air
  | Wall_torch(dir)
  | Warped_button
  | Warped_door
  | Warped_fence_gate
  | Warped_fence
  | Warped_fungus
  | Warped_hyphae
  | Warped_nylium
  | Warped_planks
  | Warped_pressure_plate
  | Warped_roots
  | Warped_sign
  | Warped_slab
  | Warped_stairs
  | Warped_stem
  | Warped_trapdoor
  | Warped_wall_sign
  | Warped_wart_block
  | Water
  | Weeping_vines
  | Weeping_vines_plant
  | Wet_sponge
  | Wheat(int)
  | White_banner
  | White_bed
  | White_carpet
  | White_concrete_powder
  | White_concrete
  | White_glazed_terracotta
  | White_shulker_box
  | White_stained_glass
  | White_stained_glass_pane
  | White_terracotta
  | White_tulip
  | White_wall_banner
  | White_wool
  | Wither_rose
  | Wither_skeleton_skull
  | Wither_skeleton_wall_skull
  | Yellow_banner
  | Yellow_bed
  | Yellow_carpet
  | Yellow_concrete_powder
  | Yellow_concrete
  | Yellow_glazed_terracotta
  | Yellow_shulker_box
  | Yellow_stained_glass
  | Yellow_stained_glass_pane
  | Yellow_terracotta
  | Yellow_wall_banner
  | Yellow_wool
  | Zombie_head
  | Zombie_wall_head;

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

let stair_dir_parse: int => stair_dir =
  fun
  | 0 => E
  | 1 => W
  | 2 => S
  | 3 => N
  | 4 => Ed
  | 5 => Wd
  | 6 => Sd
  | 7 => Nd
  | x => raise(Invalid_block(Printf.sprintf("stair dir %d", x)));

let namespace =
  fun
  | Air => "minecraft:air"
  | Flowing_water(_) => "minecraft:flowing_water"
  | Acacia_button => "minecraft:acacia_button"
  | Acacia_door => "minecraft:acacia_door"
  | Acacia_fence_gate => "minecraft:acacia_fence_gate"
  | Acacia_fence => "minecraft:acacia_fence"
  | Acacia_leaves => "minecraft:acacia_leaves"
  | Acacia_log => "minecraft:acacia_log"
  | Acacia_planks => "minecraft:acacia_planks"
  | Acacia_pressure_plate => "minecraft:acacia_pressure_plate"
  | Acacia_sapling => "minecraft:acacia_sapling"
  | Acacia_sign => "minecraft:acacia_sign"
  | Acacia_slab => "minecraft:acacia_slab"
  | Acacia_stairs => "minecraft:acacia_stairs"
  | Acacia_trapdoor => "minecraft:acacia_trapdoor"
  | Acacia_wall_sign => "minecraft:acacia_wall_sign"
  | Acacia_wood => "minecraft:acacia_wood"
  | Activator_rail => "minecraft:activator_rail"
  | Allium => "minecraft:allium"
  | Ancient_debris => "minecraft:ancient_debris"
  | Andesite => "minecraft:andesite"
  | Andesite_slab => "minecraft:andesite_slab"
  | Andesite_stairs => "minecraft:andesite_stairs"
  | Andesite_wall => "minecraft:andesite_wall"
  | Anvil => "minecraft:anvil"
  | Attached_melon_stem => "minecraft:attached_melon_stem"
  | Attached_pumpkin_stem => "minecraft:attached_pumpkin_stem"
  | Azure_bluet => "minecraft:azure_bluet"
  | Bamboo => "minecraft:bamboo"
  | Bamboo_sapling => "minecraft:bamboo_sapling"
  | Barrel => "minecraft:barrel"
  | Barrier => "minecraft:barrier"
  | Basalt => "minecraft:basalt"
  | Beacon => "minecraft:beacon"
  | Bedrock => "minecraft:bedrock"
  | Beehive => "minecraft:beehive"
  | Bee_nest => "minecraft:bee_nest"
  | Beetroots => "minecraft:beetroots"
  | Bell => "minecraft:bell"
  | Birch_button => "minecraft:birch_button"
  | Birch_door => "minecraft:birch_door"
  | Birch_fence_gate => "minecraft:birch_fence_gate"
  | Birch_fence => "minecraft:birch_fence"
  | Birch_leaves => "minecraft:birch_leaves"
  | Birch_log => "minecraft:birch_log"
  | Birch_planks => "minecraft:birch_planks"
  | Birch_pressure_plate => "minecraft:birch_pressure_plate"
  | Birch_sapling => "minecraft:birch_sapling"
  | Birch_sign => "minecraft:birch_sign"
  | Birch_slab => "minecraft:birch_slab"
  | Birch_stairs => "minecraft:birch_stairs"
  | Birch_trapdoor => "minecraft:birch_trapdoor"
  | Birch_wall_sign => "minecraft:birch_wall_sign"
  | Birch_wood => "minecraft:birch_wood"
  | Black_banner => "minecraft:black_banner"
  | Black_bed => "minecraft:black_bed"
  | Black_carpet => "minecraft:black_carpet"
  | Black_concrete_powder => "minecraft:black_concrete_powder"
  | Black_concrete => "minecraft:black_concrete"
  | Black_glazed_terracotta => "minecraft:black_glazed_terracotta"
  | Black_shulker_box => "minecraft:black_shulker_box"
  | Black_stained_glass => "minecraft:black_stained_glass"
  | Black_stained_glass_pane => "minecraft:black_stained_glass_pane"
  | Black_terracotta => "minecraft:black_terracotta"
  | Black_wall_banner => "minecraft:black_wall_banner"
  | Black_wool => "minecraft:black_wool"
  | Blackstone => "minecraft:blackstone"
  | Blackstone_slab => "minecraft:blackstone_slab"
  | Blackstone_stairs => "minecraft:blackstone_stairs"
  | Blackstone_wall => "minecraft:blackstone_wall"
  | Blast_furnace => "minecraft:blast_furnace"
  | Blue_banner => "minecraft:blue_banner"
  | Blue_bed => "minecraft:blue_bed"
  | Blue_carpet => "minecraft:blue_carpet"
  | Blue_concrete_powder => "minecraft:blue_concrete_powder"
  | Blue_concrete => "minecraft:blue_concrete"
  | Blue_glazed_terracotta => "minecraft:blue_glazed_terracotta"
  | Blue_ice => "minecraft:blue_ice"
  | Blue_orchid => "minecraft:blue_orchid"
  | Blue_shulker_box => "minecraft:blue_shulker_box"
  | Blue_stained_glass => "minecraft:blue_stained_glass"
  | Blue_stained_glass_pane => "minecraft:blue_stained_glass_pane"
  | Blue_terracotta => "minecraft:blue_terracotta"
  | Blue_wall_banner => "minecraft:blue_wall_banner"
  | Blue_wool => "minecraft:blue_wool"
  | Bone_block => "minecraft:bone_block"
  | Bookshelf => "minecraft:bookshelf"
  | Brain_coral => "minecraft:brain_coral"
  | Brain_coral_block => "minecraft:brain_coral_block"
  | Brain_coral_fan => "minecraft:brain_coral_fan"
  | Brain_coral_wall_fan => "minecraft:brain_coral_wall_fan"
  | Brewing_stand => "minecraft:brewing_stand"
  | Brick_slab => "minecraft:brick_slab"
  | Brick_stairs => "minecraft:brick_stairs"
  | Brick_wall => "minecraft:brick_wall"
  | Bricks => "minecraft:bricks"
  | Brown_banner => "minecraft:brown_banner"
  | Brown_bed => "minecraft:brown_bed"
  | Brown_carpet => "minecraft:brown_carpet"
  | Brown_concrete_powder => "minecraft:brown_concrete_powder"
  | Brown_concrete => "minecraft:brown_concrete"
  | Brown_glazed_terracotta => "minecraft:brown_glazed_terracotta"
  | Brown_mushroom_block => "minecraft:brown_mushroom_block"
  | Brown_mushroom => "minecraft:brown_mushroom"
  | Brown_shulker_box => "minecraft:brown_shulker_box"
  | Brown_stained_glass => "minecraft:brown_stained_glass"
  | Brown_stained_glass_pane => "minecraft:brown_stained_glass_pane"
  | Brown_terracotta => "minecraft:brown_terracotta"
  | Brown_wall_banner => "minecraft:brown_wall_banner"
  | Brown_wool => "minecraft:brown_wool"
  | Bubble_column => "minecraft:bubble_column"
  | Bubble_coral => "minecraft:bubble_coral"
  | Bubble_coral_block => "minecraft:bubble_coral_block"
  | Bubble_coral_fan => "minecraft:bubble_coral_fan"
  | Bubble_coral_wall_fan => "minecraft:bubble_coral_wall_fan"
  | Cactus => "minecraft:cactus"
  | Chain => "minecraft:chain"
  | Cake => "minecraft:cake"
  | Campfire => "minecraft:campfire"
  | Carrots => "minecraft:carrots"
  | Cartography_table => "minecraft:cartography_table"
  | Carved_pumpkin => "minecraft:carved_pumpkin"
  | Cauldron => "minecraft:cauldron"
  | Cave_air => "minecraft:cave_air"
  | Chain_command_block => "minecraft:chain_command_block"
  | Chest => "minecraft:chest"
  | Chipped_anvil => "minecraft:chipped_anvil"
  | Chiseled_nether_bricks => "minecraft:chiseled_nether_bricks"
  | Chiseled_polished_blackstone => "minecraft:chiseled_polished_blackstone"
  | Chiseled_quartz_block => "minecraft:chiseled_quartz_block"
  | Chiseled_red_sandstone => "minecraft:chiseled_red_sandstone"
  | Chiseled_sandstone => "minecraft:chiseled_sandstone"
  | Chiseled_stone_bricks => "minecraft:chiseled_stone_bricks"
  | Chorus_flower => "minecraft:chorus_flower"
  | Chorus_plant => "minecraft:chorus_plant"
  | Clay => "minecraft:clay"
  | Coal_block => "minecraft:coal_block"
  | Coal_ore => "minecraft:coal_ore"
  | Coarse_dirt => "minecraft:coarse_dirt"
  | Cobblestone => "minecraft:cobblestone"
  | Cobblestone_slab => "minecraft:cobblestone_slab"
  | Cobblestone_stairs(_) => "minecraft:cobblestone_stairs"
  | Cobblestone_wall => "minecraft:cobblestone_wall"
  | Cobweb => "minecraft:cobweb"
  | Cocoa => "minecraft:cocoa"
  | Command_block => "minecraft:command_block"
  | Comparator => "minecraft:comparator"
  | Composter => "minecraft:composter"
  | Conduit => "minecraft:conduit"
  | Cornflower => "minecraft:cornflower"
  | Cracked_nether_bricks => "minecraft:cracked_nether_bricks"
  | Cracked_polished_blackstone_bricks => "minecraft:cracked_polished_blackstone_bricks"
  | Cracked_stone_bricks => "minecraft:cracked_stone_bricks"
  | Crafting_table => "minecraft:crafting_table"
  | Creeper_head => "minecraft:creeper_head"
  | Creeper_wall_head => "minecraft:creeper_wall_head"
  | Crimson_button => "minecraft:crimson_button"
  | Crimson_door => "minecraft:crimson_door"
  | Crimson_fence_gate => "minecraft:crimson_fence_gate"
  | Crimson_fence => "minecraft:crimson_fence"
  | Crimson_fungus => "minecraft:crimson_fungus"
  | Crimson_hyphae => "minecraft:crimson_hyphae"
  | Crimson_nylium => "minecraft:crimson_nylium"
  | Crimson_planks => "minecraft:crimson_planks"
  | Crimson_pressure_plate => "minecraft:crimson_pressure_plate"
  | Crimson_roots => "minecraft:crimson_roots"
  | Crimson_sign => "minecraft:crimson_sign"
  | Crimson_slab => "minecraft:crimson_slab"
  | Crimson_stairs => "minecraft:crimson_stairs"
  | Crimson_stem => "minecraft:crimson_stem"
  | Crimson_trapdoor => "minecraft:crimson_trapdoor"
  | Crimson_wall_sign => "minecraft:crimson_wall_sign"
  | Crying_obsidian => "minecraft:crying_obsidian"
  | Cut_red_sandstone => "minecraft:cut_red_sandstone"
  | Cut_red_sandstone_slab => "minecraft:cut_red_sandstone_slab"
  | Cut_sandstone => "minecraft:cut_sandstone"
  | Cut_sandstone_slab => "minecraft:cut_sandstone_slab"
  | Cyan_banner => "minecraft:cyan_banner"
  | Cyan_bed => "minecraft:cyan_bed"
  | Cyan_carpet => "minecraft:cyan_carpet"
  | Cyan_concrete_powder => "minecraft:cyan_concrete_powder"
  | Cyan_concrete => "minecraft:cyan_concrete"
  | Cyan_glazed_terracotta => "minecraft:cyan_glazed_terracotta"
  | Cyan_shulker_box => "minecraft:cyan_shulker_box"
  | Cyan_stained_glass => "minecraft:cyan_stained_glass"
  | Cyan_stained_glass_pane => "minecraft:cyan_stained_glass_pane"
  | Cyan_terracotta => "minecraft:cyan_terracotta"
  | Cyan_wall_banner => "minecraft:cyan_wall_banner"
  | Cyan_wool => "minecraft:cyan_wool"
  | Damaged_anvil => "minecraft:damaged_anvil"
  | Dandelion => "minecraft:dandelion"
  | Dark_oak_button => "minecraft:dark_oak_button"
  | Dark_oak_door => "minecraft:dark_oak_door"
  | Dark_oak_fence_gate => "minecraft:dark_oak_fence_gate"
  | Dark_oak_fence => "minecraft:dark_oak_fence"
  | Dark_oak_leaves => "minecraft:dark_oak_leaves"
  | Dark_oak_log => "minecraft:dark_oak_log"
  | Dark_oak_planks => "minecraft:dark_oak_planks"
  | Dark_oak_pressure_plate => "minecraft:dark_oak_pressure_plate"
  | Dark_oak_sapling => "minecraft:dark_oak_sapling"
  | Dark_oak_sign => "minecraft:dark_oak_sign"
  | Dark_oak_slab => "minecraft:dark_oak_slab"
  | Dark_oak_stairs => "minecraft:dark_oak_stairs"
  | Dark_oak_trapdoor => "minecraft:dark_oak_trapdoor"
  | Dark_oak_wall_sign => "minecraft:dark_oak_wall_sign"
  | Dark_oak_wood => "minecraft:dark_oak_wood"
  | Dark_prismarine => "minecraft:dark_prismarine"
  | Dark_prismarine_slab => "minecraft:dark_prismarine_slab"
  | Dark_prismarine_stairs => "minecraft:dark_prismarine_stairs"
  | Daylight_detector => "minecraft:daylight_detector"
  | Dead_brain_coral => "minecraft:dead_brain_coral"
  | Dead_brain_coral_block => "minecraft:dead_brain_coral_block"
  | Dead_brain_coral_fan => "minecraft:dead_brain_coral_fan"
  | Dead_brain_coral_wall_fan => "minecraft:dead_brain_coral_wall_fan"
  | Dead_bubble_coral => "minecraft:dead_bubble_coral"
  | Dead_bubble_coral_block => "minecraft:dead_bubble_coral_block"
  | Dead_bubble_coral_fan => "minecraft:dead_bubble_coral_fan"
  | Dead_bubble_coral_wall_fan => "minecraft:dead_bubble_coral_wall_fan"
  | Dead_bush => "minecraft:dead_bush"
  | Dead_fire_coral => "minecraft:dead_fire_coral"
  | Dead_fire_coral_block => "minecraft:dead_fire_coral_block"
  | Dead_fire_coral_fan => "minecraft:dead_fire_coral_fan"
  | Dead_fire_coral_wall_fan => "minecraft:dead_fire_coral_wall_fan"
  | Dead_horn_coral => "minecraft:dead_horn_coral"
  | Dead_horn_coral_block => "minecraft:dead_horn_coral_block"
  | Dead_horn_coral_fan => "minecraft:dead_horn_coral_fan"
  | Dead_horn_coral_wall_fan => "minecraft:dead_horn_coral_wall_fan"
  | Dead_tube_coral => "minecraft:dead_tube_coral"
  | Dead_tube_coral_block => "minecraft:dead_tube_coral_block"
  | Dead_tube_coral_fan => "minecraft:dead_tube_coral_fan"
  | Dead_tube_coral_wall_fan => "minecraft:dead_tube_coral_wall_fan"
  | Detector_rail => "minecraft:detector_rail"
  | Diamond_block => "minecraft:diamond_block"
  | Diamond_ore => "minecraft:diamond_ore"
  | Diorite => "minecraft:diorite"
  | Diorite_slab => "minecraft:diorite_slab"
  | Diorite_stairs => "minecraft:diorite_stairs"
  | Diorite_wall => "minecraft:diorite_wall"
  | Dirt => "minecraft:dirt"
  | Dispenser => "minecraft:dispenser"
  | Dragon_egg => "minecraft:dragon_egg"
  | Dragon_head => "minecraft:dragon_head"
  | Dragon_wall_head => "minecraft:dragon_wall_head"
  | Dried_kelp_block => "minecraft:dried_kelp_block"
  | Dropper => "minecraft:dropper"
  | Emerald_block => "minecraft:emerald_block"
  | Emerald_ore => "minecraft:emerald_ore"
  | Enchanting_table => "minecraft:enchanting_table"
  | End_gateway => "minecraft:end_gateway"
  | End_portal_frame => "minecraft:end_portal_frame"
  | End_portal => "minecraft:end_portal"
  | End_rod => "minecraft:end_rod"
  | End_stone => "minecraft:end_stone"
  | End_stone_brick_slab => "minecraft:end_stone_brick_slab"
  | End_stone_brick_stairs => "minecraft:end_stone_brick_stairs"
  | End_stone_brick_wall => "minecraft:end_stone_brick_wall"
  | End_stone_bricks => "minecraft:end_stone_bricks"
  | Ender_chest => "minecraft:ender_chest"
  | Farmland => "minecraft:farmland"
  | Fern => "minecraft:fern"
  | Fire => "minecraft:fire"
  | Fire_coral => "minecraft:fire_coral"
  | Fire_coral_block => "minecraft:fire_coral_block"
  | Fire_coral_fan => "minecraft:fire_coral_fan"
  | Fire_coral_wall_fan => "minecraft:fire_coral_wall_fan"
  | Fletching_table => "minecraft:fletching_table"
  | Flower_pot => "minecraft:flower_pot"
  | Frosted_ice => "minecraft:frosted_ice"
  | Furnace => "minecraft:furnace"
  | Gilded_blackstone => "minecraft:gilded_blackstone"
  | Glass => "minecraft:glass"
  | Glass_pane => "minecraft:glass_pane"
  | Glowstone => "minecraft:glowstone"
  | Gold_block => "minecraft:gold_block"
  | Gold_ore => "minecraft:gold_ore"
  | Granite => "minecraft:granite"
  | Granite_slab => "minecraft:granite_slab"
  | Granite_stairs => "minecraft:granite_stairs"
  | Granite_wall => "minecraft:granite_wall"
  | Grass_block => "minecraft:grass_block"
  | Grass_path => "minecraft:grass_path"
  | Grass => "minecraft:grass"
  | Gravel => "minecraft:gravel"
  | Gray_banner => "minecraft:gray_banner"
  | Gray_bed => "minecraft:gray_bed"
  | Gray_carpet => "minecraft:gray_carpet"
  | Gray_concrete_powder => "minecraft:gray_concrete_powder"
  | Gray_concrete => "minecraft:gray_concrete"
  | Gray_glazed_terracotta => "minecraft:gray_glazed_terracotta"
  | Gray_shulker_box => "minecraft:gray_shulker_box"
  | Gray_stained_glass => "minecraft:gray_stained_glass"
  | Gray_stained_glass_pane => "minecraft:gray_stained_glass_pane"
  | Gray_terracotta => "minecraft:gray_terracotta"
  | Gray_wall_banner => "minecraft:gray_wall_banner"
  | Gray_wool => "minecraft:gray_wool"
  | Green_banner => "minecraft:green_banner"
  | Green_bed => "minecraft:green_bed"
  | Green_carpet => "minecraft:green_carpet"
  | Green_concrete_powder => "minecraft:green_concrete_powder"
  | Green_concrete => "minecraft:green_concrete"
  | Green_glazed_terracotta => "minecraft:green_glazed_terracotta"
  | Green_shulker_box => "minecraft:green_shulker_box"
  | Green_stained_glass => "minecraft:green_stained_glass"
  | Green_stained_glass_pane => "minecraft:green_stained_glass_pane"
  | Green_terracotta => "minecraft:green_terracotta"
  | Green_wall_banner => "minecraft:green_wall_banner"
  | Green_wool => "minecraft:green_wool"
  | Grindstone => "minecraft:grindstone"
  | Hay_block => "minecraft:hay_block"
  | Heavy_weighted_pressure_plate => "minecraft:heavy_weighted_pressure_plate"
  | Hopper => "minecraft:hopper"
  | Honey_block => "minecraft:honey_block"
  | Honeycomb_block => "minecraft:honeycomb_block"
  | Horn_coral => "minecraft:horn_coral"
  | Horn_coral_block => "minecraft:horn_coral_block"
  | Horn_coral_fan => "minecraft:horn_coral_fan"
  | Horn_coral_wall_fan => "minecraft:horn_coral_wall_fan"
  | Ice => "minecraft:ice"
  | Infested_chiseled_stone_bricks => "minecraft:infested_chiseled_stone_bricks"
  | Infested_cobblestone => "minecraft:infested_cobblestone"
  | Infested_cracked_stone_bricks => "minecraft:infested_cracked_stone_bricks"
  | Infested_mossy_stone_bricks => "minecraft:infested_mossy_stone_bricks"
  | Infested_stone => "minecraft:infested_stone"
  | Infested_stone_bricks => "minecraft:infested_stone_bricks"
  | Iron_bars => "minecraft:iron_bars"
  | Iron_door => "minecraft:iron_door"
  | Iron_block => "minecraft:iron_block"
  | Iron_ore => "minecraft:iron_ore"
  | Iron_trapdoor => "minecraft:iron_trapdoor"
  | Jack_o_lantern => "minecraft:jack_o_lantern"
  | Jigsaw => "minecraft:jigsaw"
  | Jukebox => "minecraft:jukebox"
  | Jungle_button => "minecraft:jungle_button"
  | Jungle_door => "minecraft:jungle_door"
  | Jungle_fence_gate => "minecraft:jungle_fence_gate"
  | Jungle_fence => "minecraft:jungle_fence"
  | Jungle_leaves => "minecraft:jungle_leaves"
  | Jungle_log => "minecraft:jungle_log"
  | Jungle_planks => "minecraft:jungle_planks"
  | Jungle_pressure_plate => "minecraft:jungle_pressure_plate"
  | Jungle_sapling => "minecraft:jungle_sapling"
  | Jungle_sign => "minecraft:jungle_sign"
  | Jungle_slab => "minecraft:jungle_slab"
  | Jungle_stairs => "minecraft:jungle_stairs"
  | Jungle_trapdoor => "minecraft:jungle_trapdoor"
  | Jungle_wall_sign => "minecraft:jungle_wall_sign"
  | Jungle_wood => "minecraft:jungle_wood"
  | Kelp => "minecraft:kelp"
  | Kelp_plant => "minecraft:kelp_plant"
  | Ladder => "minecraft:ladder"
  | Lantern => "minecraft:lantern"
  | Lapis_block => "minecraft:lapis_block"
  | Lapis_ore => "minecraft:lapis_ore"
  | Large_fern => "minecraft:large_fern"
  | Lava => "minecraft:lava"
  | Lectern => "minecraft:lectern"
  | Lever => "minecraft:lever"
  | Light_blue_banner => "minecraft:light_blue_banner"
  | Light_blue_bed => "minecraft:light_blue_bed"
  | Light_blue_carpet => "minecraft:light_blue_carpet"
  | Light_blue_concrete_powder => "minecraft:light_blue_concrete_powder"
  | Light_blue_concrete => "minecraft:light_blue_concrete"
  | Light_blue_glazed_terracotta => "minecraft:light_blue_glazed_terracotta"
  | Light_blue_shulker_box => "minecraft:light_blue_shulker_box"
  | Light_blue_stained_glass => "minecraft:light_blue_stained_glass"
  | Light_blue_stained_glass_pane => "minecraft:light_blue_stained_glass_pane"
  | Light_blue_terracotta => "minecraft:light_blue_terracotta"
  | Light_blue_wall_banner => "minecraft:light_blue_wall_banner"
  | Light_blue_wool => "minecraft:light_blue_wool"
  | Light_gray_banner => "minecraft:light_gray_banner"
  | Light_gray_bed => "minecraft:light_gray_bed"
  | Light_gray_carpet => "minecraft:light_gray_carpet"
  | Light_gray_concrete_powder => "minecraft:light_gray_concrete_powder"
  | Light_gray_concrete => "minecraft:light_gray_concrete"
  | Light_gray_glazed_terracotta => "minecraft:light_gray_glazed_terracotta"
  | Light_gray_shulker_box => "minecraft:light_gray_shulker_box"
  | Light_gray_stained_glass => "minecraft:light_gray_stained_glass"
  | Light_gray_stained_glass_pane => "minecraft:light_gray_stained_glass_pane"
  | Light_gray_terracotta => "minecraft:light_gray_terracotta"
  | Light_gray_wall_banner => "minecraft:light_gray_wall_banner"
  | Light_gray_wool => "minecraft:light_gray_wool"
  | Light_weighted_pressure_plate => "minecraft:light_weighted_pressure_plate"
  | Lilac => "minecraft:lilac"
  | Lily_pad => "minecraft:lily_pad"
  | Lily_of_the_valley => "minecraft:lily_of_the_valley"
  | Lime_banner => "minecraft:lime_banner"
  | Lime_bed => "minecraft:lime_bed"
  | Lime_carpet => "minecraft:lime_carpet"
  | Lime_concrete_powder => "minecraft:lime_concrete_powder"
  | Lime_concrete => "minecraft:lime_concrete"
  | Lime_glazed_terracotta => "minecraft:lime_glazed_terracotta"
  | Lime_shulker_box => "minecraft:lime_shulker_box"
  | Lime_stained_glass => "minecraft:lime_stained_glass"
  | Lime_stained_glass_pane => "minecraft:lime_stained_glass_pane"
  | Lime_terracotta => "minecraft:lime_terracotta"
  | Lime_wall_banner => "minecraft:lime_wall_banner"
  | Lime_wool => "minecraft:lime_wool"
  | Lodestone => "minecraft:lodestone"
  | Loom => "minecraft:loom"
  | Magenta_banner => "minecraft:magenta_banner"
  | Magenta_bed => "minecraft:magenta_bed"
  | Magenta_carpet => "minecraft:magenta_carpet"
  | Magenta_concrete_powder => "minecraft:magenta_concrete_powder"
  | Magenta_concrete => "minecraft:magenta_concrete"
  | Magenta_glazed_terracotta => "minecraft:magenta_glazed_terracotta"
  | Magenta_shulker_box => "minecraft:magenta_shulker_box"
  | Magenta_stained_glass => "minecraft:magenta_stained_glass"
  | Magenta_stained_glass_pane => "minecraft:magenta_stained_glass_pane"
  | Magenta_terracotta => "minecraft:magenta_terracotta"
  | Magenta_wall_banner => "minecraft:magenta_wall_banner"
  | Magenta_wool => "minecraft:magenta_wool"
  | Magma_block => "minecraft:magma_block"
  | Melon => "minecraft:melon"
  | Melon_stem => "minecraft:melon_stem"
  | Mossy_cobblestone => "minecraft:mossy_cobblestone"
  | Mossy_cobblestone_slab => "minecraft:mossy_cobblestone_slab"
  | Mossy_cobblestone_stairs => "minecraft:mossy_cobblestone_stairs"
  | Mossy_cobblestone_wall => "minecraft:mossy_cobblestone_wall"
  | Mossy_stone_brick_slab => "minecraft:mossy_stone_brick_slab"
  | Mossy_stone_brick_stairs => "minecraft:mossy_stone_brick_stairs"
  | Mossy_stone_brick_wall => "minecraft:mossy_stone_brick_wall"
  | Mossy_stone_bricks => "minecraft:mossy_stone_bricks"
  | Moving_piston => "minecraft:moving_piston"
  | Mushroom_stem => "minecraft:mushroom_stem"
  | Mycelium => "minecraft:mycelium"
  | Nether_brick_fence => "minecraft:nether_brick_fence"
  | Nether_brick_slab => "minecraft:nether_brick_slab"
  | Nether_brick_stairs => "minecraft:nether_brick_stairs"
  | Nether_brick_wall => "minecraft:nether_brick_wall"
  | Nether_bricks => "minecraft:nether_bricks"
  | Nether_gold_ore => "minecraft:nether_gold_ore"
  | Nether_portal => "minecraft:nether_portal"
  | Nether_quartz_ore => "minecraft:nether_quartz_ore"
  | Nether_sprouts => "minecraft:nether_sprouts"
  | Nether_wart_block => "minecraft:nether_wart_block"
  | Nether_wart => "minecraft:nether_wart"
  | Netherite_block => "minecraft:netherite_block"
  | Netherrack => "minecraft:netherrack"
  | Note_block => "minecraft:note_block"
  | Oak_button => "minecraft:oak_button"
  | Oak_door(_, _) => "minecraft:oak_door"
  | Oak_fence_gate => "minecraft:oak_fence_gate"
  | Oak_fence => "minecraft:oak_fence"
  | Oak_leaves => "minecraft:oak_leaves"
  | Oak_log(_) => "minecraft:oak_log"
  | Oak_planks => "minecraft:oak_planks"
  | Oak_pressure_plate => "minecraft:oak_pressure_plate"
  | Oak_sapling => "minecraft:oak_sapling"
  | Oak_sign => "minecraft:oak_sign"
  | Oak_slab => "minecraft:oak_slab"
  | Oak_stairs => "minecraft:oak_stairs"
  | Oak_trapdoor => "minecraft:oak_trapdoor"
  | Oak_wall_sign => "minecraft:oak_wall_sign"
  | Oak_wood => "minecraft:oak_wood"
  | Observer => "minecraft:observer"
  | Obsidian => "minecraft:obsidian"
  | Orange_banner => "minecraft:orange_banner"
  | Orange_bed(_, _) => "minecraft:orange_bed"
  | Orange_carpet => "minecraft:orange_carpet"
  | Orange_concrete_powder => "minecraft:orange_concrete_powder"
  | Orange_concrete => "minecraft:orange_concrete"
  | Orange_glazed_terracotta => "minecraft:orange_glazed_terracotta"
  | Orange_shulker_box => "minecraft:orange_shulker_box"
  | Orange_stained_glass => "minecraft:orange_stained_glass"
  | Orange_stained_glass_pane => "minecraft:orange_stained_glass_pane"
  | Orange_terracotta => "minecraft:orange_terracotta"
  | Orange_tulip => "minecraft:orange_tulip"
  | Orange_wall_banner => "minecraft:orange_wall_banner"
  | Orange_wool => "minecraft:orange_wool"
  | Oxeye_daisy => "minecraft:oxeye_daisy"
  | Packed_ice => "minecraft:packed_ice"
  | Peony => "minecraft:peony"
  | Petrified_oak_slab => "minecraft:petrified_oak_slab"
  | Pink_banner => "minecraft:pink_banner"
  | Pink_bed => "minecraft:pink_bed"
  | Pink_carpet => "minecraft:pink_carpet"
  | Pink_concrete_powder => "minecraft:pink_concrete_powder"
  | Pink_concrete => "minecraft:pink_concrete"
  | Pink_glazed_terracotta => "minecraft:pink_glazed_terracotta"
  | Pink_shulker_box => "minecraft:pink_shulker_box"
  | Pink_stained_glass => "minecraft:pink_stained_glass"
  | Pink_stained_glass_pane => "minecraft:pink_stained_glass_pane"
  | Pink_terracotta => "minecraft:pink_terracotta"
  | Pink_tulip => "minecraft:pink_tulip"
  | Pink_wall_banner => "minecraft:pink_wall_banner"
  | Pink_wool => "minecraft:pink_wool"
  | Piston_head => "minecraft:piston_head"
  | Piston => "minecraft:piston"
  | Player_head => "minecraft:player_head"
  | Player_wall_head => "minecraft:player_wall_head"
  | Podzol => "minecraft:podzol"
  | Polished_andesite => "minecraft:polished_andesite"
  | Polished_andesite_slab => "minecraft:polished_andesite_slab"
  | Polished_andesite_stairs => "minecraft:polished_andesite_stairs"
  | Polished_basalt => "minecraft:polished_basalt"
  | Polished_blackstone => "minecraft:polished_blackstone"
  | Polished_blackstone_brick_slab => "minecraft:polished_blackstone_brick_slab"
  | Polished_blackstone_brick_stairs => "minecraft:polished_blackstone_brick_stairs"
  | Polished_blackstone_brick_wall => "minecraft:polished_blackstone_brick_wall"
  | Polished_blackstone_bricks => "minecraft:polished_blackstone_bricks"
  | Polished_blackstone_button => "minecraft:polished_blackstone_button"
  | Polished_blackstone_pressure_plate => "minecraft:polished_blackstone_pressure_plate"
  | Polished_blackstone_slab => "minecraft:polished_blackstone_slab"
  | Polished_blackstone_stairs => "minecraft:polished_blackstone_stairs"
  | Polished_blackstone_wall => "minecraft:polished_blackstone_wall"
  | Polished_diorite => "minecraft:polished_diorite"
  | Polished_diorite_slab => "minecraft:polished_diorite_slab"
  | Polished_diorite_stairs => "minecraft:polished_diorite_stairs"
  | Polished_granite => "minecraft:polished_granite"
  | Polished_granite_slab => "minecraft:polished_granite_slab"
  | Polished_granite_stairs => "minecraft:polished_granite_stairs"
  | Poppy => "minecraft:poppy"
  | Potatoes => "minecraft:potatoes"
  | Potted_acacia_sapling => "minecraft:potted_acacia_sapling"
  | Potted_allium => "minecraft:potted_allium"
  | Potted_azure_bluet => "minecraft:potted_azure_bluet"
  | Potted_bamboo => "minecraft:potted_bamboo"
  | Potted_birch_sapling => "minecraft:potted_birch_sapling"
  | Potted_blue_orchid => "minecraft:potted_blue_orchid"
  | Potted_brown_mushroom => "minecraft:potted_brown_mushroom"
  | Potted_cactus => "minecraft:potted_cactus"
  | Potted_cornflower => "minecraft:potted_cornflower"
  | Potted_crimson_fungus => "minecraft:potted_crimson_fungus"
  | Potted_crimson_roots => "minecraft:potted_crimson_roots"
  | Potted_dandelion => "minecraft:potted_dandelion"
  | Potted_dark_oak_sapling => "minecraft:potted_dark_oak_sapling"
  | Potted_dead_bush => "minecraft:potted_dead_bush"
  | Potted_fern => "minecraft:potted_fern"
  | Potted_jungle_sapling => "minecraft:potted_jungle_sapling"
  | Potted_lily_of_the_valley => "minecraft:potted_lily_of_the_valley"
  | Potted_oak_sapling => "minecraft:potted_oak_sapling"
  | Potted_orange_tulip => "minecraft:potted_orange_tulip"
  | Potted_oxeye_daisy => "minecraft:potted_oxeye_daisy"
  | Potted_pink_tulip => "minecraft:potted_pink_tulip"
  | Potted_poppy => "minecraft:potted_poppy"
  | Potted_red_mushroom => "minecraft:potted_red_mushroom"
  | Potted_red_tulip => "minecraft:potted_red_tulip"
  | Potted_spruce_sapling => "minecraft:potted_spruce_sapling"
  | Potted_warped_fungus => "minecraft:potted_warped_fungus"
  | Potted_warped_roots => "minecraft:potted_warped_roots"
  | Potted_white_tulip => "minecraft:potted_white_tulip"
  | Potted_wither_rose => "minecraft:potted_wither_rose"
  | Powered_rail => "minecraft:powered_rail"
  | Prismarine => "minecraft:prismarine"
  | Prismarine_brick_slab => "minecraft:prismarine_brick_slab"
  | Prismarine_brick_stairs => "minecraft:prismarine_brick_stairs"
  | Prismarine_bricks => "minecraft:prismarine_bricks"
  | Prismarine_slab => "minecraft:prismarine_slab"
  | Prismarine_stairs => "minecraft:prismarine_stairs"
  | Prismarine_wall => "minecraft:prismarine_wall"
  | Pumpkin => "minecraft:pumpkin"
  | Pumpkin_stem => "minecraft:pumpkin_stem"
  | Purple_banner => "minecraft:purple_banner"
  | Purple_bed => "minecraft:purple_bed"
  | Purple_carpet => "minecraft:purple_carpet"
  | Purple_concrete_powder => "minecraft:purple_concrete_powder"
  | Purple_concrete => "minecraft:purple_concrete"
  | Purple_glazed_terracotta => "minecraft:purple_glazed_terracotta"
  | Purple_shulker_box => "minecraft:purple_shulker_box"
  | Purple_stained_glass => "minecraft:purple_stained_glass"
  | Purple_stained_glass_pane => "minecraft:purple_stained_glass_pane"
  | Purple_terracotta => "minecraft:purple_terracotta"
  | Purple_wall_banner => "minecraft:purple_wall_banner"
  | Purple_wool => "minecraft:purple_wool"
  | Purpur_block => "minecraft:purpur_block"
  | Purpur_pillar => "minecraft:purpur_pillar"
  | Purpur_slab => "minecraft:purpur_slab"
  | Purpur_stairs => "minecraft:purpur_stairs"
  | Quartz_block => "minecraft:quartz_block"
  | Quartz_bricks => "minecraft:quartz_bricks"
  | Quartz_pillar => "minecraft:quartz_pillar"
  | Quartz_slab => "minecraft:quartz_slab"
  | Quartz_stairs => "minecraft:quartz_stairs"
  | Rail => "minecraft:rail"
  | Red_banner => "minecraft:red_banner"
  | Red_bed => "minecraft:red_bed"
  | Red_carpet => "minecraft:red_carpet"
  | Red_concrete_powder => "minecraft:red_concrete_powder"
  | Red_concrete => "minecraft:red_concrete"
  | Red_glazed_terracotta => "minecraft:red_glazed_terracotta"
  | Red_mushroom_block => "minecraft:red_mushroom_block"
  | Red_mushroom => "minecraft:red_mushroom"
  | Red_nether_brick_slab => "minecraft:red_nether_brick_slab"
  | Red_nether_brick_stairs => "minecraft:red_nether_brick_stairs"
  | Red_nether_brick_wall => "minecraft:red_nether_brick_wall"
  | Red_nether_bricks => "minecraft:red_nether_bricks"
  | Red_sand => "minecraft:red_sand"
  | Red_sandstone => "minecraft:red_sandstone"
  | Red_sandstone_slab => "minecraft:red_sandstone_slab"
  | Red_sandstone_stairs => "minecraft:red_sandstone_stairs"
  | Red_sandstone_wall => "minecraft:red_sandstone_wall"
  | Red_shulker_box => "minecraft:red_shulker_box"
  | Red_stained_glass => "minecraft:red_stained_glass"
  | Red_stained_glass_pane => "minecraft:red_stained_glass_pane"
  | Red_terracotta => "minecraft:red_terracotta"
  | Red_tulip => "minecraft:red_tulip"
  | Red_wall_banner => "minecraft:red_wall_banner"
  | Red_wool => "minecraft:red_wool"
  | Respawn_anchor => "minecraft:respawn_anchor"
  | Redstone_block => "minecraft:redstone_block"
  | Redstone_lamp => "minecraft:redstone_lamp"
  | Redstone_ore => "minecraft:redstone_ore"
  | Redstone_torch => "minecraft:redstone_torch"
  | Redstone_wall_torch => "minecraft:redstone_wall_torch"
  | Redstone_wire => "minecraft:redstone_wire"
  | Repeater => "minecraft:repeater"
  | Repeating_command_block => "minecraft:repeating_command_block"
  | Rose_bush => "minecraft:rose_bush"
  | Sand => "minecraft:sand"
  | Sandstone => "minecraft:sandstone"
  | Sandstone_slab => "minecraft:sandstone_slab"
  | Sandstone_stairs => "minecraft:sandstone_stairs"
  | Sandstone_wall => "minecraft:sandstone_wall"
  | Scaffolding => "minecraft:scaffolding"
  | Sea_lantern => "minecraft:sea_lantern"
  | Sea_pickle => "minecraft:sea_pickle"
  | Seagrass => "minecraft:seagrass"
  | Shroomlight => "minecraft:shroomlight"
  | Shulker_box => "minecraft:shulker_box"
  | Skeleton_skull => "minecraft:skeleton_skull"
  | Skeleton_wall_skull => "minecraft:skeleton_wall_skull"
  | Slime_block => "minecraft:slime_block"
  | Smithing_table => "minecraft:smithing_table"
  | Smoker => "minecraft:smoker"
  | Smooth_quartz => "minecraft:smooth_quartz"
  | Smooth_quartz_slab => "minecraft:smooth_quartz_slab"
  | Smooth_quartz_stairs => "minecraft:smooth_quartz_stairs"
  | Smooth_red_sandstone => "minecraft:smooth_red_sandstone"
  | Smooth_red_sandstone_slab => "minecraft:smooth_red_sandstone_slab"
  | Smooth_red_sandstone_stairs => "minecraft:smooth_red_sandstone_stairs"
  | Smooth_sandstone => "minecraft:smooth_sandstone"
  | Smooth_sandstone_slab => "minecraft:smooth_sandstone_slab"
  | Smooth_sandstone_stairs => "minecraft:smooth_sandstone_stairs"
  | Smooth_stone => "minecraft:smooth_stone"
  | Smooth_stone_slab(_) => "minecraft:smooth_stone_slab"
  | Snow_block => "minecraft:snow_block"
  | Snow => "minecraft:snow"
  | Soul_campfire => "minecraft:soul_campfire"
  | Soul_fire => "minecraft:soul_fire"
  | Soul_lantern => "minecraft:soul_lantern"
  | Soul_sand => "minecraft:soul_sand"
  | Soul_soil => "minecraft:soul_soil"
  | Soul_torch => "minecraft:soul_torch"
  | Soul_wall_torch => "minecraft:soul_wall_torch"
  | Spawner => "minecraft:spawner"
  | Sponge => "minecraft:sponge"
  | Spruce_button => "minecraft:spruce_button"
  | Spruce_door => "minecraft:spruce_door"
  | Spruce_fence_gate => "minecraft:spruce_fence_gate"
  | Spruce_fence => "minecraft:spruce_fence"
  | Spruce_leaves => "minecraft:spruce_leaves"
  | Spruce_log => "minecraft:spruce_log"
  | Spruce_planks => "minecraft:spruce_planks"
  | Spruce_pressure_plate => "minecraft:spruce_pressure_plate"
  | Spruce_sapling => "minecraft:spruce_sapling"
  | Spruce_sign => "minecraft:spruce_sign"
  | Spruce_slab => "minecraft:spruce_slab"
  | Spruce_stairs => "minecraft:spruce_stairs"
  | Spruce_trapdoor => "minecraft:spruce_trapdoor"
  | Spruce_wall_sign => "minecraft:spruce_wall_sign"
  | Spruce_wood => "minecraft:spruce_wood"
  | Sticky_piston => "minecraft:sticky_piston"
  | Stone => "minecraft:stone"
  | Stone_brick_slab => "minecraft:stone_brick_slab"
  | Stone_brick_stairs(_) => "minecraft:stone_brick_stairs"
  | Stone_brick_wall => "minecraft:stone_brick_wall"
  | Stone_bricks => "minecraft:stone_bricks"
  | Stone_button => "minecraft:stone_button"
  | Stone_pressure_plate => "minecraft:stone_pressure_plate"
  | Stone_slab => "minecraft:stone_slab"
  | Stone_stairs(_) => "minecraft:stone_stairs"
  | Stonecutter => "minecraft:stonecutter"
  | Stripped_acacia_log => "minecraft:stripped_acacia_log"
  | Stripped_acacia_wood => "minecraft:stripped_acacia_wood"
  | Stripped_birch_log => "minecraft:stripped_birch_log"
  | Stripped_birch_wood => "minecraft:stripped_birch_wood"
  | Stripped_crimson_hyphae => "minecraft:stripped_crimson_hyphae"
  | Stripped_crimson_stem => "minecraft:stripped_crimson_stem"
  | Stripped_dark_oak_log => "minecraft:stripped_dark_oak_log"
  | Stripped_dark_oak_wood => "minecraft:stripped_dark_oak_wood"
  | Stripped_jungle_log => "minecraft:stripped_jungle_log"
  | Stripped_jungle_wood => "minecraft:stripped_jungle_wood"
  | Stripped_oak_log => "minecraft:stripped_oak_log"
  | Stripped_oak_wood => "minecraft:stripped_oak_wood"
  | Stripped_spruce_log => "minecraft:stripped_spruce_log"
  | Stripped_spruce_wood => "minecraft:stripped_spruce_wood"
  | Stripped_warped_hyphae => "minecraft:stripped_warped_hyphae"
  | Stripped_warped_stem => "minecraft:stripped_warped_stem"
  | Structure_block => "minecraft:structure_block"
  | Structure_void => "minecraft:structure_void"
  | Sugar_cane => "minecraft:sugar_cane"
  | Sunflower => "minecraft:sunflower"
  | Sweet_berry_bush => "minecraft:sweet_berry_bush"
  | Tall_grass => "minecraft:tall_grass"
  | Tall_seagrass => "minecraft:tall_seagrass"
  | Target => "minecraft:target"
  | Terracotta => "minecraft:terracotta"
  | Tnt => "minecraft:tnt"
  | Torch => "minecraft:torch"
  | Trapped_chest => "minecraft:trapped_chest"
  | Tripwire_hook => "minecraft:tripwire_hook"
  | Tripwire => "minecraft:tripwire"
  | Tube_coral => "minecraft:tube_coral"
  | Tube_coral_block => "minecraft:tube_coral_block"
  | Tube_coral_fan => "minecraft:tube_coral_fan"
  | Tube_coral_wall_fan => "minecraft:tube_coral_wall_fan"
  | Turtle_egg => "minecraft:turtle_egg"
  | Twisting_vines => "minecraft:twisting_vines"
  | Twisting_vines_plant => "minecraft:twisting_vines_plant"
  | Vine => "minecraft:vine"
  | Void_air => "minecraft:void_air"
  | Wall_torch(_) => "minecraft:wall_torch"
  | Warped_button => "minecraft:warped_button"
  | Warped_door => "minecraft:warped_door"
  | Warped_fence_gate => "minecraft:warped_fence_gate"
  | Warped_fence => "minecraft:warped_fence"
  | Warped_fungus => "minecraft:warped_fungus"
  | Warped_hyphae => "minecraft:warped_hyphae"
  | Warped_nylium => "minecraft:warped_nylium"
  | Warped_planks => "minecraft:warped_planks"
  | Warped_pressure_plate => "minecraft:warped_pressure_plate"
  | Warped_roots => "minecraft:warped_roots"
  | Warped_sign => "minecraft:warped_sign"
  | Warped_slab => "minecraft:warped_slab"
  | Warped_stairs => "minecraft:warped_stairs"
  | Warped_stem => "minecraft:warped_stem"
  | Warped_trapdoor => "minecraft:warped_trapdoor"
  | Warped_wall_sign => "minecraft:warped_wall_sign"
  | Warped_wart_block => "minecraft:warped_wart_block"
  | Water => "minecraft:water"
  | Weeping_vines => "minecraft:weeping_vines"
  | Weeping_vines_plant => "minecraft:weeping_vines_plant"
  | Wet_sponge => "minecraft:wet_sponge"
  | Wheat(_) => "minecraft:wheat"
  | White_banner => "minecraft:white_banner"
  | White_bed => "minecraft:white_bed"
  | White_carpet => "minecraft:white_carpet"
  | White_concrete_powder => "minecraft:white_concrete_powder"
  | White_concrete => "minecraft:white_concrete"
  | White_glazed_terracotta => "minecraft:white_glazed_terracotta"
  | White_shulker_box => "minecraft:white_shulker_box"
  | White_stained_glass => "minecraft:white_stained_glass"
  | White_stained_glass_pane => "minecraft:white_stained_glass_pane"
  | White_terracotta => "minecraft:white_terracotta"
  | White_tulip => "minecraft:white_tulip"
  | White_wall_banner => "minecraft:white_wall_banner"
  | White_wool => "minecraft:white_wool"
  | Wither_rose => "minecraft:wither_rose"
  | Wither_skeleton_skull => "minecraft:wither_skeleton_skull"
  | Wither_skeleton_wall_skull => "minecraft:wither_skeleton_wall_skull"
  | Yellow_banner => "minecraft:yellow_banner"
  | Yellow_bed => "minecraft:yellow_bed"
  | Yellow_carpet => "minecraft:yellow_carpet"
  | Yellow_concrete_powder => "minecraft:yellow_concrete_powder"
  | Yellow_concrete => "minecraft:yellow_concrete"
  | Yellow_glazed_terracotta => "minecraft:yellow_glazed_terracotta"
  | Yellow_shulker_box => "minecraft:yellow_shulker_box"
  | Yellow_stained_glass => "minecraft:yellow_stained_glass"
  | Yellow_stained_glass_pane => "minecraft:yellow_stained_glass_pane"
  | Yellow_terracotta => "minecraft:yellow_terracotta"
  | Yellow_wall_banner => "minecraft:yellow_wall_banner"
  | Yellow_wool => "minecraft:yellow_wool"
  | Zombie_head => "minecraft:zombie_head"
  | Zombie_wall_head => "minecraft:zombie_wall_head";

let string_of_dir =
  fun
  | N => "north"
  | E => "east"
  | S => "south"
  | W => "west";

let string_of_axis =
  fun
  | X => "x"
  | Y => "y"
  | Z => "z";

let data = block => {
  open Nbt.Node;
  let properties =
    switch (block) {
    | Farmland => ["moisture" >: String("7")]
    | Flowing_water(level) => ["level" >: String(string_of_int(level))]
    | Oak_door(dir, part) => [
        "facing" >: String(string_of_dir(dir)),
        "half"
        >: String(
             switch (part) {
             | Lower => "lower"
             | Upper => "upper"
             },
           ),
      ]
    | Oak_log(axis) => ["axis" >: String(string_of_axis(axis))]
    | Orange_bed(dir, part) => [
        "facing" >: String(string_of_dir(dir)),
        "part"
        >: String(
             switch (part) {
             | Foot => "foot"
             | Head => "head"
             },
           ),
      ]
    | Smooth_stone_slab(t) => [
        "type"
        >: String(
             switch (t) {
             | Bottom => "bottom"
             | Top => "top"
             | Double => "double"
             },
           ),
        "waterlogged" >: String("false"),
      ]
    | Snow => ["layers" >: String("1")]
    | Cobblestone_stairs(dir)
    | Stone_brick_stairs(dir)
    | Stone_stairs(dir) => [
        "facing"
        >: String(
             switch (dir) {
             | N
             | Nd => "north"
             | E
             | Ed => "east"
             | S
             | Sd => "south"
             | W
             | Wd => "west"
             },
           ),
        "half"
        >: String(
             switch (dir) {
             | N
             | E
             | S
             | W => "bottom"
             | Nd
             | Ed
             | Sd
             | Wd => "top"
             },
           ),
        "shape" >: String("straight"),
        "waterlogged" >: String("false"),
      ]
    | Wall_torch(dir) => ["facing" >: String(string_of_dir(dir))]
    | Wheat(age) => ["age" >: String(string_of_int(age))]
    | _ => []
    };
  Compound(properties);
};

let block_entity = block => {
  Nbt.Node.(
    switch (block) {
    | Bell => Some(["id" >: String("bell")])
    | Orange_bed(_, _) => Some(["id" >: String("bed")])
    | _ => None
    }
  );
};