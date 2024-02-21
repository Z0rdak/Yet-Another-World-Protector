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
import de.z0rdak.yawp.util.StickType;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.util.text.event.ClickEvent;
import net.minecraftforge.event.entity.player.AnvilRepairEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;

import static de.z0rdak.yawp.commands.CommandConstants.CREATE;
import static de.z0rdak.yawp.commands.CommandConstants.MARKER;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.buildCommandStr;
import static de.z0rdak.yawp.util.MessageUtil.*;
import static de.z0rdak.yawp.util.StickUtil.*;

public class MarkerStickHandler {

    public static void onMarkBlock(ItemStack involvedItem, PlayerInteractEvent.RightClickBlock event) {
        MarkerStick marker = new MarkerStick(involvedItem.getTag().getCompound(STICK));
        AreaType areaType = marker.getAreaType();
        if (areaType == null) {
            YetAnotherWorldProtector.LOGGER.error("Unknown area type on marking - should really not happening");
            throw new IllegalArgumentException("Unexpected value. AreaType is null");
        }
        if (event.getPlayer().isShiftKeyDown()) {
            marker.setTeleportPos(event.getPos());
            involvedItem.getTag().put(STICK, marker.serializeNBT());
            setStickName(involvedItem, StickType.MARKER);
            sendMessage(event.getPlayer(), new TranslationTextComponent("cli.marker.create.mark.tp-pos", shortBlockPos(event.getPos())));
            return;
        }
        // add block to NBT list
        int index = marker.getMarkedBlocks().size() % (marker.getAreaType().neededBlocks + 1);
        marker.addMarkedBlock(event.getPos());
        sendMessage(event.getPlayer(), new TranslationTextComponent("cli.marker.create.mark.block", index, shortBlockPos(event.getPos())));
        // check whether marked blocks form a valid marked area
        boolean hasValidArea = marker.checkValidArea();
        involvedItem.getTag().put(STICK, marker.serializeNBT());
        setStickName(involvedItem, StickType.MARKER);
        if (hasValidArea) {
            // send info about valid area and how to create a region
            IFormattableTextComponent cmdText = new TranslationTextComponent("cli.marker.create.link.text");
            IFormattableTextComponent cmdHoverText = new TranslationTextComponent("cli.marker.create.link.hover");
            String cmd = buildCommandStr(MARKER.toString(), CREATE.toString(), "");
            IFormattableTextComponent markerCmdSuggestionLink = buildExecuteCmdComponent(cmdText, cmdHoverText, cmd, ClickEvent.Action.SUGGEST_COMMAND, SUGGEST_COLOR);
            sendMessage(event.getPlayer(), new TranslationTextComponent("cli.marker.create.mark.valid", markerCmdSuggestionLink));
        }
    }

    /**
     * Create a region from the NBT data of the renamed region marker.
     */
    public static void onCreateRegion(AnvilRepairEvent event) {
        ItemStack outputItem = event.getItemResult();
        PlayerEntity player = event.getPlayer();
        CompoundNBT stickNBT = outputItem.getTag().getCompound(STICK);
        StickType type = StickType.of(stickNBT.getString(STICK_TYPE));
        if (type == StickType.MARKER) {
            String regionName = outputItem.getHoverName().getString();
            MarkerStick marker = new MarkerStick(stickNBT);
            if (marker.isValidArea()) {
                AbstractMarkableRegion region = LocalRegions.regionFrom(player, marker, regionName);
                if (region != null) {
                    DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(player.getCommandSenderWorld().dimension());
                    if (dimCache != null){
                        dimCache.addRegion(region);
                        LocalRegions.ensureHigherRegionPriorityFor((CuboidRegion) region, RegionConfig.getDefaultPriority());
                        marker.reset();
                        outputItem.getTag().put(STICK, marker.serializeNBT());
                        setStickName(outputItem, type);
                        RegionDataManager.save();
                    } else {
                        sendMessage(player, new TranslationTextComponent("Player dimension not matching marker data"));
                    }
                } else {
                    sendMessage(player, new TranslationTextComponent("Invalid region type"));
                }
            } else {
                sendMessage(player, new TranslationTextComponent("Could not create region"));
            }
        } else {
            sendMessage(player, new TranslationTextComponent("Invalid stick type / NBT data"));
        }
    }

    public static void onCycleRegionMarker(ItemStack markerStickItem){
        CompoundNBT nbt = markerStickItem.getTag();
        MarkerStick marker = new MarkerStick(nbt.getCompound(STICK));
        // change area nbt, reset marked blocks, set valid to false
        marker.cycleMode();
        // update stick name
        markerStickItem.getTag().put(STICK, marker.serializeNBT());
        StickUtil.setStickName(markerStickItem, StickType.MARKER);
    }
}
