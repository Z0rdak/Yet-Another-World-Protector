package de.z0rdak.yawp.platform.event;

import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.api.events.flag.FlagEvents;
import net.fabricmc.fabric.api.event.Event;
import net.fabricmc.fabric.api.event.EventFactory;



public final class FabricFlagEvents {

    private FabricFlagEvents() {
    }

    public static final Event<FlagEvents.AddFlagListener> ADD_FLAG =
            EventFactory.createArrayBacked(FlagEvents.AddFlagListener.class, callbacks -> (addFlagEvent) -> {
                for (FlagEvents.AddFlagListener callback : callbacks) callback.add(addFlagEvent);
            });

    public static final Event<FlagEvents.RemoveFlagListener> REMOVE_FLAG =
            EventFactory.createArrayBacked(FlagEvents.RemoveFlagListener.class, callbacks -> (removeFlagEvent) -> {
                for (FlagEvents.RemoveFlagListener callback : callbacks) callback.remove(removeFlagEvent);
            });

    public static final Event<FlagEvents.UpdateFlagMessageListener> UPDATE_FLAG_MSG =
            EventFactory.createArrayBacked(FlagEvents.UpdateFlagMessageListener.class, callbacks -> (updateFlagMsgEvent) -> {
                FlagEvent.UpdateFlagMessage res = updateFlagMsgEvent;
                for (FlagEvents.UpdateFlagMessageListener callback : callbacks) {
                    res = callback.updateMessage(updateFlagMsgEvent);
                }
                return res;
            });

    public static final Event<FlagEvents.FlagCheckListener> CHECK_FLAG =
            EventFactory.createArrayBacked(FlagEvents.FlagCheckListener.class,
                    callbacks -> (event) -> {
                        boolean canceled = false;
                        for (FlagEvents.FlagCheckListener callback : callbacks) {
                            if (!callback.checkFlag(event)) canceled = true;
                        }
                        return canceled;
                    });

    public static final Event<FlagEvents.FlagResultListener> FLAG_RESULT =
            EventFactory.createArrayBacked(FlagEvents.FlagResultListener.class,
                    callbacks -> (result) -> {
                        for (FlagEvents.FlagResultListener callback : callbacks) {
                            result = callback.getResult(result);
                        }
                        return result;
                    });
}