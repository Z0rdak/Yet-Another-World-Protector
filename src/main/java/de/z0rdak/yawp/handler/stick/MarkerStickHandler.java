package de.z0rdak.yawp.handler.stick;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.region.AbstractMarkableRegion;
import de.z0rdak.yawp.core.region.CuboidRegion;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.MessageUtil;
import de.z0rdak.yawp.util.StickType;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.event.entity.player.AnvilRepairEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;

import static de.z0rdak.yawp.util.StickUtil.*;

public class MarkerStickHandler {

    public static void onMarkBlock(ItemStack involvedItem, PlayerInteractEvent.RightClickBlock event) {
        MarkerStick marker = new MarkerStick(involvedItem.getTag().getCompound(STICK));
        AreaType areaType = marker.getAreaType();
        if (areaType == null) {
            YetAnotherWorldProtector.LOGGER.warn("Unknown area type on marking - should really not happening");
            return;
        }
        if (event.getEntity().isShiftKeyDown()) {
            marker.setTeleportPos(event.getPos());
            involvedItem.getTag().put(STICK, marker.serializeNBT());
            return;
        }
        // add block to NBT list
        marker.addMarkedBlock(event.getPos());
        // check whether marked blocks form a valid marked area
        marker.checkValidArea();
        involvedItem.getTag().put(STICK, marker.serializeNBT());
        setStickName(involvedItem, StickType.MARKER);
    }


    /**
     * Create a region from the NBT data of the renamed region marker.
     * TODO: Make parent selectable for creating region, this is needed to check if a player is allowed to create a region with the stick
     */
    public static void onCreateRegion(AnvilRepairEvent event) {
        ItemStack outputItem = event.getOutput();
        Player player = event.getEntity();
        CompoundTag stickNBT = outputItem.getTag().getCompound(STICK);
        StickType type = StickType.of(stickNBT.getString(STICK_TYPE));
        if (type == StickType.MARKER) {
            String regionName = outputItem.getHoverName().getString();
            MarkerStick marker = new MarkerStick(stickNBT);
            if (marker.isValidArea()) {
                AbstractMarkableRegion region = LocalRegions.regionFrom(player, marker, regionName);
                if (region != null) {
                    DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(player.getCommandSenderWorld().dimension());
                    if (dimCache != null) {
                        dimCache.addRegion(region);
                        LocalRegions.ensureHigherRegionPriorityFor((CuboidRegion) region, RegionConfig.DEFAULT_REGION_PRIORITY.get());
                        marker.reset();
                        outputItem.getTag().put(STICK, marker.serializeNBT());
                        setStickName(outputItem, type);
                        // TODO: Reset marker on dimChange?
                        RegionDataManager.save();
                    } else {
                        MessageUtil.sendMessage(player, Component.translatableWithFallback("Player dimension not matching marker data", "Player dimension not matching marker data"));
                    }
                } else {
                    MessageUtil.sendMessage(player, Component.translatableWithFallback("Invalid region type", "Invalid region type"));
                }
            } else {
                MessageUtil.sendMessage(player, Component.translatableWithFallback("Could not create region", "Could not create region"));
            }
        } else {
            MessageUtil.sendMessage(player, Component.translatableWithFallback("Invalid stick type / NBT data", "Invalid stick type / NBT data"));
        }
    }

    public static void onCycleRegionMarker(ItemStack markerStickItem) {
        CompoundTag nbt = markerStickItem.getTag();
        MarkerStick marker = new MarkerStick(nbt.getCompound(STICK));
        // change area nbt, reset marked blocks, set valid to false
        marker.cycleMode();
        // update stick name
        markerStickItem.getTag().put(STICK, marker.serializeNBT());
        StickUtil.setStickName(markerStickItem, StickType.MARKER);
    }
}
