package de.z0rdak.yawp.platform.event;

import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.fabricmc.fabric.api.event.Event;
import net.fabricmc.fabric.api.event.EventFactory;
import net.minecraft.server.level.ServerPlayer;

import static com.ibm.icu.impl.ValidIdentifiers.Datatype.region;

/**
 * Fabric-specific implementation of region events for YAWP.
 *
 * <p>This class defines Fabric {@link Event}s corresponding to the
 * cross-platform region events defined in {@link RegionEvents}. It
 * provides hooks for modloader-specific code (Fabric) to register
 * listeners while propagating events to the common event bus so that
 * common or mod-independent code can also respond.</p>
 *
 * <p>Events available:
 * <ul>
 *   <li>{@link #CREATE_REGION} – fired when a region is created</li>
 *   <li>{@link #DELETE_REGION} – fired when a region is deleted</li>
 *   <li>{@link #RENAME_REGION} – fired when a region is renamed</li>
 *   <li>{@link #UPDATE_AREA} – fired when a region’s area is updated</li>
 *   <li>{@link #ON_PLAYER_ENTER_REGION} – fired when a player enters a region</li>
 *   <li>{@link #ON_PLAYER_LEAVE_REGION} – fired when a player leaves a region</li>
 * </ul>
 *
 * <p>Listeners registered here are called first, then the events are
 * propagated to {@link RegionEvents}’ common event bus.</p>
 *
 * <p>Usage:
 * <pre>{@code
 * FabricRegionEvents.CREATE_REGION.register(event -> {
 *     // handle region creation
 *     return true; // or false to cancel
 * });
 * }</pre>
 */
public final class FabricRegionEvents {

    public static final Event<RegionEvents.CreateRegionListener> CREATE_REGION = EventFactory.createArrayBacked(RegionEvents.CreateRegionListener.class, callbacks -> (createRegionEvent) -> {
        for (RegionEvents.CreateRegionListener callback : callbacks) {
            if (!callback.create(createRegionEvent)) {
                Constants.LOGGER.info("3 in post fabric event abort");
                return true;
            }
        }
        Constants.LOGGER.info("3 in post fabric event ok");
        return false;
    });
    public static final Event<RegionEvents.RemoveRegionListener> DELETE_REGION = EventFactory.createArrayBacked(RegionEvents.RemoveRegionListener.class, callbacks -> (removeRegionEvent) -> {
        for (RegionEvents.RemoveRegionListener callback : callbacks) {
            if (!callback.remove(removeRegionEvent)) {
                return true;
            }
        }
        return false;
    });
    public static final Event<RegionEvents.RenameRegionListener> RENAME_REGION = EventFactory.createArrayBacked(RegionEvents.RenameRegionListener.class, callbacks -> (renameRegionEvent) -> {
        for (RegionEvents.RenameRegionListener callback : callbacks) {
            if (!callback.rename(renameRegionEvent)) {
                return true;
            }
        }
        return false;
    });

    public static final Event<RegionEvents.UpdateAreaListener> UPDATE_AREA = EventFactory.createArrayBacked(RegionEvents.UpdateAreaListener.class, callbacks -> (updateAreaEvent) -> {
        for (RegionEvents.UpdateAreaListener callback : callbacks) {
            if (!callback.update(updateAreaEvent)) {
                return true;
            }
        }
        return false;
    });

    public static final Event<RegionEvents.PlayerRegionEnterListener> ON_PLAYER_ENTER_REGION =
            EventFactory.createArrayBacked(RegionEvents.PlayerRegionEnterListener.class,
                    listeners -> (onEnter) -> {
                        for (RegionEvents.PlayerRegionEnterListener l : listeners) {
                            l.onEnter(onEnter);
                        }
                    });

    public static final Event<RegionEvents.PlayerRegionLeaveListener> ON_PLAYER_LEAVE_REGION =
            EventFactory.createArrayBacked(RegionEvents.PlayerRegionLeaveListener.class,
                    listeners -> (onLeave) -> {
                        for (RegionEvents.PlayerRegionLeaveListener l : listeners) {
                            l.onLeave(onLeave);
                        }
                    });


    private FabricRegionEvents() {
    }
}