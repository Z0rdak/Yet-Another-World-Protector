package de.z0rdak.yawp.api.events.flag;

import de.z0rdak.yawp.platform.services.event.EventBus;

public final class FlagEvents {

    public static final EventBus<FlagCheckListener> ON_FLAG_CHECK = new EventBus<>();
    public static final EventBus<FlagResultListener> ON_FLAG_RESULT = new EventBus<>();

    public static final EventBus<AddFlagListener> ON_ADD_FLAG = new EventBus<>();
    public static final EventBus<RemoveFlagListener> ON_REMOVE_FLAG = new EventBus<>();
    public static final EventBus<UpdateFlagMessageListener> ON_UPDATE_FLAG_MESSAGE = new EventBus<>();

    private FlagEvents() {}

    @FunctionalInterface
    public interface FlagResultListener {
        FlagCheckResult getResult(FlagCheckResult flagCheckResult);
    }

    @FunctionalInterface
    public interface FlagCheckListener {
        boolean checkFlag(FlagCheckRequest flagCheckRequest);
    }

    @FunctionalInterface
    public interface AddFlagListener {
        boolean add(FlagEvent.Add event);
    }

    @FunctionalInterface
    public interface RemoveFlagListener {
        boolean remove(FlagEvent.Remove event);
    }

    @FunctionalInterface
    public interface UpdateFlagMessageListener {
        FlagEvent.UpdateFlagMessage updateMessage(FlagEvent.UpdateFlagMessage event);
    }
}
