package de.z0rdak.yawp.platform.event;

import de.z0rdak.yawp.api.events.region.RegionEvents;
import net.fabricmc.fabric.api.event.Event;
import net.fabricmc.fabric.api.event.EventFactory;

import static de.z0rdak.yawp.api.events.Cancelable.CANCEL;
import static de.z0rdak.yawp.api.events.Cancelable.CONTINUE;

public final class FabricRegionEvents {

    public static final Event<RegionEvents.CreateRegionListener> CREATE_REGION =
            EventFactory.createArrayBacked(RegionEvents.CreateRegionListener.class, callbacks -> (createRegionEvent) -> {
                for (RegionEvents.CreateRegionListener callback : callbacks) {
                    if (!callback.create(createRegionEvent) || createRegionEvent.isCanceled()) {
                        return CANCEL;
                    }
                }
                return CONTINUE;
            });

    public static final Event<RegionEvents.RemoveRegionListener> DELETE_REGION =
            EventFactory.createArrayBacked(RegionEvents.RemoveRegionListener.class, callbacks -> (removeRegionEvent) -> {
                for (RegionEvents.RemoveRegionListener callback : callbacks) {
                    if (!callback.remove(removeRegionEvent) || removeRegionEvent.isCanceled()) {
                        return CANCEL;
                    }
                }
                return CONTINUE;
            });

    public static final Event<RegionEvents.RenameRegionListener> RENAME_REGION =
            EventFactory.createArrayBacked(RegionEvents.RenameRegionListener.class, callbacks -> (renameRegionEvent) -> {
                for (RegionEvents.RenameRegionListener callback : callbacks) {
                    if (!callback.rename(renameRegionEvent) || renameRegionEvent.isCanceled()) {
                        return CANCEL;
                    }
                }
                return CONTINUE;
            });

    public static final Event<RegionEvents.UpdateAreaListener> UPDATE_AREA =
            EventFactory.createArrayBacked(RegionEvents.UpdateAreaListener.class, callbacks -> (updateAreaEvent) -> {
                for (RegionEvents.UpdateAreaListener callback : callbacks) {
                    if (!callback.update(updateAreaEvent) || updateAreaEvent.isCanceled()) {
                        return CANCEL;
                    }
                }
                return CONTINUE;
            });

    public static final Event<RegionEvents.PlayerRegionEnterListener> ON_PLAYER_ENTER_REGION =
            EventFactory.createArrayBacked(RegionEvents.PlayerRegionEnterListener.class, listeners -> (onEnter) -> {
                for (RegionEvents.PlayerRegionEnterListener l : listeners) {
                    if (!l.onEnter(onEnter) || onEnter.isCanceled()) {
                        return CANCEL;
                    }
                }
                return CONTINUE;
            });

    public static final Event<RegionEvents.PlayerRegionLeaveListener> ON_PLAYER_LEAVE_REGION =
            EventFactory.createArrayBacked(RegionEvents.PlayerRegionLeaveListener.class, listeners -> (onLeave) -> {
                for (RegionEvents.PlayerRegionLeaveListener l : listeners) {
                    if (!l.onLeave(onLeave) || onLeave.isCanceled()) {
                        return CANCEL;
                    }
                }
                return CONTINUE;
            });

    private FabricRegionEvents() {
    }
}