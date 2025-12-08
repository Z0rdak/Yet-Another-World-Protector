package de.z0rdak.yawp;

import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.api.events.flag.FlagEvents;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.constants.Constants;

import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.handler.RegionSpatialCache;
import de.z0rdak.yawp.handler.YawpEventHandler;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.ChatComponentBuilder;
import net.minecraft.server.level.ServerPlayer;

public class YAWPCommon {

    private YAWPCommon() {}

    public static void init() {
        Constants.LOGGER.debug("[{}] Loading {} in a {} environment!", Constants.MOD_ID, Services.PLATFORM.getPlatformName(), Services.PLATFORM.getEnvironmentName());

        FlagEvents.ON_ADD_FLAG.register(YawpEventHandler::onAddFlag);
        RegionEvents.ON_UPDATE_AREA.register(YawpEventHandler::onUpdateRegion);
        RegionEvents.ON_REMOVE.register(YawpEventHandler::onRemoveRegion);

        RegionEvents.ON_PLAYER_ENTER_REGION.register(YawpEventHandler::onPlayerEnterRegion);
        RegionEvents.ON_PLAYER_LEAVE_REGION.register(YawpEventHandler::onPlayerLeaveRegion);

        RegionEvents.ON_CREATE.register(RegionSpatialCache::onCreateRegion);
        RegionEvents.ON_REMOVE.register(RegionSpatialCache::onRemoveRegion);
        RegionEvents.ON_UPDATE_AREA.register(RegionSpatialCache::onUpdateRegion);

        /* Debug loggers */
        RegionEvents.ON_CREATE.register(YAWPCommon::onCreateRegion);
        RegionEvents.ON_REMOVE.register(YAWPCommon::onRemoveRegion);
        RegionEvents.ON_UPDATE_AREA.register(YAWPCommon::onUpdateRegion);
        RegionEvents.ON_PLAYER_ENTER_REGION.register(YAWPCommon::onEnterRegion);
        RegionEvents.ON_PLAYER_LEAVE_REGION.register(YAWPCommon::onLeaveRegion);
    }


    private static void onEnterRegion(RegionEvent.PlayerEnter onEnter) {
        Constants.LOGGER.debug("Player {} entered region {} in {} at {}",
                onEnter.getPlayer(), onEnter.getRegion().getName(),
                onEnter.getRegion().getDim().location().toString(),
                ChatComponentBuilder.tinyBlockPos(onEnter.getPlayer().blockPosition()));
    }

    private static void onLeaveRegion(RegionEvent.PlayerLeave onLeave) {
        Constants.LOGGER.debug("Player {} left region {} in {} at {}",
                onLeave.getPlayer(), onLeave.getRegion().getName(),
                onLeave.getRegion().getDim().location().toString(),
                ChatComponentBuilder.tinyBlockPos(onLeave.getPlayer().blockPosition()));
    }

    public static boolean onCreateRegion(RegionEvent.Create create) {
        ServerPlayer player = create.getPlayer();
        var regionLoc = create.getRegion().getDim().location().toString();
        var regionName = create.getRegion().getName();
        if (player == null) {
            Constants.LOGGER.debug("Region '{}' was created in '{}'.", regionName, regionLoc);
        } else {
            Constants.LOGGER.debug("Region '{}' was created in '{}' by '{}'.", regionName, regionLoc, player.getScoreboardName());
        }
        return true;
    }

    public static boolean onRemoveRegion(RegionEvent.Remove remove) {
        ServerPlayer player = remove.getPlayer();
        var regionLoc = remove.getRegion().getDim().location().toString();
        var regionName = remove.getRegion().getName();
        if (player == null) {
            Constants.LOGGER.debug("Region '{}' in '{}' was deleted.", regionName, regionLoc);
        } else {
            Constants.LOGGER.debug("Region '{}' in '{}' was deleted by '{}'.", regionName, regionLoc, player.getScoreboardName());
        }
        return true;
    }

    public static boolean onUpdateRegion(RegionEvent.UpdateArea update) {
        ServerPlayer player = update.getPlayer();
        var regionLoc = update.getRegion().getDim().location().toString();
        var regionName = update.getRegion().getName();
        if (player == null) {
            Constants.LOGGER.debug("Region area of '{}' in '{}' was updated.", regionName, regionLoc);
        } else {
            Constants.LOGGER.debug("Region area of '{}' in '{}' was updated by '{}'.", regionName, regionLoc, player.getScoreboardName());
        }
        return true;
    }
}