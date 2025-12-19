package de.z0rdak.yawp.api.events.region;

import de.z0rdak.yawp.platform.services.event.EventBus;

public final class RegionEvents {

    @FunctionalInterface public interface CreateRegionListener {
        /**
         *
         * @param event
         * @return false to cancel the region creation, true otherwise
         */
        boolean create(RegionEvent.Create event);
    }

    @FunctionalInterface public interface RemoveRegionListener {

        /**
         *
         * @param event
         * @return false to cancel the region removal, true otherwise
         */
        boolean remove(RegionEvent.Remove event);
    }

    @FunctionalInterface public interface RenameRegionListener {
        /**
         *
         * @param event
         * @return false to cancel the region renaming, true otherwise
         */
        boolean rename(RegionEvent.Rename event);
    }

    @FunctionalInterface public interface UpdateAreaListener {

        /**
         *
         * @param event
         * @return false to cancel the region area update, true otherwise
         */
        boolean update(RegionEvent.UpdateArea event);
    }

    @FunctionalInterface
    public interface PlayerRegionLeaveListener {
        /**
         * Cancelation not yet implemented
         * @param onLeave
         * @return false to prevent player from entering the region, true otherwise
         */
        boolean onLeave(RegionEvent.PlayerLeave onLeave);
    }

    @FunctionalInterface
    public interface PlayerRegionEnterListener {
        /**
         * Cancelation not yet implemented
         * @param onEnter
         * @return false to prevent player from leaving the region, true otherwise
         */
        boolean onEnter(RegionEvent.PlayerEnter onEnter);
    }

    public static final EventBus<CreateRegionListener> ON_CREATE = new EventBus<>();
    public static final EventBus<RemoveRegionListener> ON_REMOVE = new EventBus<>();
    public static final EventBus<RenameRegionListener> ON_RENAME = new EventBus<>();
    public static final EventBus<UpdateAreaListener> ON_UPDATE_AREA = new EventBus<>();

    public static final EventBus<PlayerRegionLeaveListener> ON_PLAYER_LEAVE_REGION = new EventBus<>();
    public static final EventBus<PlayerRegionEnterListener> ON_PLAYER_ENTER_REGION = new EventBus<>();

}