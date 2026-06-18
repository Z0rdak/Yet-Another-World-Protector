package de.z0rdak.yawp;

import de.z0rdak.yawp.api.events.flag.FlagEvents;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.handler.RegionSpatialCache;
import de.z0rdak.yawp.handler.YawpEventHandler;

public class YAWPCommon {

    // TODO mod loader overwrite for modloader info
    public static final String VERSION = "26.2-0.6.4-beta1";

    private YAWPCommon() {}

    public static void init() {
        FlagEvents.ON_ADD_FLAG.register(YawpEventHandler::onAddFlag);
        RegionEvents.ON_UPDATE_AREA.register(YawpEventHandler::onUpdateRegion);
        RegionEvents.ON_REMOVE.register(YawpEventHandler::onRemoveRegion);

        RegionEvents.ON_CREATE.register(RegionSpatialCache::onCreateRegion);
        RegionEvents.ON_REMOVE.register(RegionSpatialCache::onRemoveRegion);
        RegionEvents.ON_UPDATE_AREA.register(RegionSpatialCache::onUpdateRegion);
    }
}