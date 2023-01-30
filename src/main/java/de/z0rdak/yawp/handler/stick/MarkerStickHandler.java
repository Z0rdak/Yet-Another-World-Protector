package de.z0rdak.yawp.handler.stick;

public class MarkerStickHandler {

    /*
    public static void onMarkBlock(ItemStack involvedItem, PlayerInteractEvent.RightClickBlock event) {
        MarkerStick marker = new MarkerStick(involvedItem.getNbt().getCompound(STICK));
        AreaType areaType = marker.getAreaType();
        if (areaType == null) {
            YetAnotherWorldProtector.LOGGER.warn("Unknown area type on marking - should really not happening");
            return;
        }
        if (event.getPlayer().isShiftKeyDown()) {
            marker.setTeleportPos(event.getPos());
            involvedItem.getNbt().put(STICK, marker.serializeNBT());
            return;
        }
        // add block to NBT list
        marker.addMarkedBlock(event.getPos());
        // check whether marked blocks form a valid marked area
        marker.checkValidArea();
        involvedItem.getNbt().put(STICK, marker.serializeNBT());
        setStickName(involvedItem, StickType.MARKER);
    }


    /**
     * Create a region from the NBT data of the renamed region marker.
     * TODO: Make parent selectable for creating region, this is needed to check if a player is allowed to create a region with the stick

    public static void onCreateRegion(AnvilRepairEvent event) {
        ItemStack outputItem = event.getItemResult();
        PlayerEntity player = event.getPlayer();
        NbtCompound stickNBT = outputItem.getNbt().getCompound(STICK);
        StickType type = StickType.of(stickNBT.getString(STICK_TYPE));
        if (type == StickType.MARKER) {
            String regionName = outputItem.getName().getString();
            MarkerStick marker = new MarkerStick(stickNBT);
            if (marker.isValidArea()) {
                AbstractMarkableRegion region = LocalRegions.regionFrom(player, marker, regionName);
                if (region != null) {
                    DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(player.getWorld().getRegistryKey());
                    if (dimCache != null){
                        dimCache.addRegion(region);
                        LocalRegions.ensureHigherRegionPriorityFor((CuboidRegion) region, RegionConfig.DEFAULT_REGION_PRIORITY.get());
                        marker.reset();
                        outputItem.getNbt().put(STICK, marker.serializeNBT());
                        setStickName(outputItem, type);
                        // TODO: Reset marker on dimChange?
                        RegionDataManager.save();
                    } else {
                        sendMessage(player, MutableText.of(new TranslatableTextContent("PlayerEntity dimension not matching marker data")));
                    }
                } else {
                    player.sendMessage(MutableText.of(new TranslatableTextContent("Invalid region type")));
                }
            } else {
                player.sendMessage(MutableText.of(new TranslatableTextContent("Could not create region")));
            }
        } else {
            player.sendMessage(MutableText.of(new TranslatableTextContent("Invalid stick type / NBT data")));
        }
    }

    public static void onCycleRegionMarker(ItemStack markerStickItem){
        NbtCompound nbt = markerStickItem.getNbt();
        MarkerStick marker = new MarkerStick(nbt.getCompound(STICK));
        // change area nbt, reset marked blocks, set valid to false
        marker.cycleMode();
        // update stick name
        markerStickItem.getNbt().put(STICK, marker.serializeNBT());
        StickUtil.setStickName(markerStickItem, StickType.MARKER);
    }

     */
}
