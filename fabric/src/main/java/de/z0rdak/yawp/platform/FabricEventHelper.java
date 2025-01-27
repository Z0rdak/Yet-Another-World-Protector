package de.z0rdak.yawp.platform;

import de.z0rdak.yawp.api.events.flag.FabricFlagEvents;
import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.api.events.region.FabricRegionEvents;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.platform.services.IEventHelper;

public class FabricEventHelper implements IEventHelper {

    @Override
    public boolean post(FlagCheckEvent event) {
        return FabricRegionEvents.CHECK_FLAG.invoker().checkFlag(event);
    }

    @Override
    public FlagCheckResult post(FlagCheckResult event) {
        return FabricRegionEvents.FLAG_RESULT.invoker().getResult(event);
    }

    @Override
    public boolean post(RegionEvent event) {
        if (event instanceof RegionEvent.Create) {
            return FabricRegionEvents.CREATE_REGION.invoker().createRegion((RegionEvent.Create) event);
        }
        if (event instanceof RegionEvent.Rename) {
            return FabricRegionEvents.RENAME_REGION.invoker().renameRegion((RegionEvent.Rename) event);
        }
        if (event instanceof RegionEvent.Remove) {
            return FabricRegionEvents.DELETE_REGION.invoker().deleteRegion((RegionEvent.Remove) event);
        }
        if (event instanceof RegionEvent.UpdateArea) {
            post((RegionEvent.UpdateArea) event);
        }
        return false;
    }

    @Override
    public RegionEvent.UpdateArea post(RegionEvent.UpdateArea event) {
        FabricRegionEvents.UPDATE_AREA.invoker().updateArea(event);
        return event;
    }

    @Override
    public void post(FlagEvent event) {
        if (event instanceof FlagEvent.RemoveFlagEvent) {
            FabricFlagEvents.REMOVE_FLAG.invoker().removeFlag((FlagEvent.RemoveFlagEvent) event);
        }
        if (event instanceof FlagEvent.AddFlagEvent) {
            FabricFlagEvents.ADD_FLAG.invoker().addFlag((FlagEvent.AddFlagEvent) event);
        }
        if (event instanceof FlagEvent.UpdateFlagMessageEvent) {
            FabricFlagEvents.UPDATE_FLAG_MSG.invoker().updateFlagMsg((FlagEvent.UpdateFlagMessageEvent) event);
        }
    }

    @Override
    public FlagEvent.UpdateFlagMessageEvent post(FlagEvent.UpdateFlagMessageEvent event) {
        return FabricFlagEvents.UPDATE_FLAG_MSG.invoker().updateFlagMsg(event);
    }

}