package de.z0rdak.yawp.platform.event;

import de.z0rdak.yawp.api.events.Cancelable;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.platform.services.event.RegionEventDispatcher;

/**
 * Fabric-specific backend for {@link RegionEventDispatcher}.
 * Integrates Fabric callback events with the common region event system.
 */
public final class FabricRegionEventDispatcher implements RegionEventDispatcher {

    @Override
    public <T extends RegionEvent> boolean post(T event) {
        boolean canceled = false;

        if (event instanceof RegionEvent.Create e) {
            canceled = FabricRegionEvents.CREATE_REGION.invoker().create(e) != Cancelable.CONTINUE;
            e.setCanceled(canceled);
            RegionEvents.ON_CREATE.invoke(cb -> cb.create(e));
        } else if (event instanceof RegionEvent.Rename e) {
            canceled = FabricRegionEvents.RENAME_REGION.invoker().rename(e) != Cancelable.CONTINUE;
            e.setCanceled(canceled);
            RegionEvents.ON_RENAME.invoke(cb -> cb.rename(e));
        } else if (event instanceof RegionEvent.UpdateArea e) {
            canceled = FabricRegionEvents.UPDATE_AREA.invoker().update(e) != Cancelable.CONTINUE;
            e.setCanceled(canceled);
            RegionEvents.ON_UPDATE_AREA.invoke(cb -> cb.update(e));
        } else if (event instanceof RegionEvent.Remove e) {
            canceled = FabricRegionEvents.DELETE_REGION.invoker().remove(e) != Cancelable.CONTINUE;
            e.setCanceled(canceled);
            RegionEvents.ON_REMOVE.invoke(cb -> cb.remove(e));
        } else if (event instanceof RegionEvent.PlayerEnter e) {
            canceled = FabricRegionEvents.ON_PLAYER_ENTER_REGION.invoker().onEnter(e) != Cancelable.CONTINUE;
            e.setCanceled(canceled);
            RegionEvents.ON_PLAYER_ENTER_REGION.invoke(cb -> cb.onEnter(e));
        } else if (event instanceof RegionEvent.PlayerLeave e) {
            canceled = FabricRegionEvents.ON_PLAYER_LEAVE_REGION.invoker().onLeave(e) != Cancelable.CONTINUE;
            e.setCanceled(canceled);
            RegionEvents.ON_PLAYER_LEAVE_REGION.invoke(cb -> cb.onLeave(e));
        }
        return canceled;
    }

}
