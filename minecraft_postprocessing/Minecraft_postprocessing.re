open! Core_kernel;

module Fence = Fence;
module Water = Water;

let run_all = (r: Minecraft.Region.t): unit => {
  Fence.connect_fences(r);
  Water.flow_water(r);
};