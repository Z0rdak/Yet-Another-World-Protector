package de.z0rdak.yawp.api.events.region;

import de.z0rdak.yawp.platform.services.event.EventBus;

public final class RegionEvents {
    @FunctionalInterface public interface CreateRegionListener {
        boolean create(RegionEvent.Create event);
    }

    @FunctionalInterface public interface RemoveRegionListener {
        boolean remove(RegionEvent.Remove event);
    }

    @FunctionalInterface public interface RenameRegionListener {
        boolean rename(RegionEvent.Rename event);
    }

    @FunctionalInterface public interface UpdateAreaListener {
        boolean update(RegionEvent.UpdateArea event);
    }

    @FunctionalInterface
    public interface PlayerRegionLeaveListener {
        void onLeave(RegionEvent.PlayerLeave onLeave);
    }

    @FunctionalInterface
    public interface PlayerRegionEnterListener {
        void onEnter(RegionEvent.PlayerEnter onEnter);
    }

    public static final EventBus<CreateRegionListener> ON_CREATE = new EventBus<>();
    public static final EventBus<RemoveRegionListener> ON_REMOVE = new EventBus<>();
    public static final EventBus<RenameRegionListener> ON_RENAME = new EventBus<>();
    public static final EventBus<UpdateAreaListener> ON_UPDATE_AREA = new EventBus<>();

    public static final EventBus<PlayerRegionLeaveListener> ON_PLAYER_LEAVE_REGION = new EventBus<>();
    public static final EventBus<PlayerRegionEnterListener> ON_PLAYER_ENTER_REGION = new EventBus<>();

}