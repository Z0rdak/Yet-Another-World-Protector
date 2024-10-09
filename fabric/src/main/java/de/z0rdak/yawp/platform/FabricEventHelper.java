package de.z0rdak.yawp.platform;

import de.z0rdak.yawp.api.events.region.FabricRegionEvents;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.platform.services.IEventHelper;

public class FabricEventHelper implements IEventHelper {

    @Override
    public boolean post(FlagCheckEvent event) {
        return FabricRegionEvents.post(event);
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
            return FabricRegionEvents.UPDATE_AREA.invoker().updateArea((RegionEvent.UpdateArea) event);
        }
        return false;
    }

}