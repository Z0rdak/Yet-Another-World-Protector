package de.z0rdak.yawp.api.events.flag;

import net.fabricmc.fabric.api.event.Event;
import net.fabricmc.fabric.api.event.EventFactory;

public final class FabricFlagEvents {

    public static final Event<AddFlag> ADD_FLAG = EventFactory.createArrayBacked(AddFlag.class, callbacks -> (addFlagEvent) -> {
        for (AddFlag callback : callbacks) {
            callback.addFlag(addFlagEvent);
        }
    });
    public static final Event<RemoveFlag> REMOVE_FLAG = EventFactory.createArrayBacked(RemoveFlag.class, callbacks -> (removeFlagEvent) -> {
        for (RemoveFlag callback : callbacks) {
            callback.removeFlag(removeFlagEvent);
        }
    });
    public static final Event<UpdateFlagMessage> UPDATE_FLAG_MSG = EventFactory.createArrayBacked(UpdateFlagMessage.class, callbacks -> (updateFlagMsgEvent) -> {
        FlagEvent.UpdateFlagMessageEvent res = updateFlagMsgEvent;
        for (UpdateFlagMessage callback : callbacks) {
            res = callback.updateFlagMsg(updateFlagMsgEvent);
        }
        return res;
    });

    private FabricFlagEvents() {
    }

    @FunctionalInterface
    public interface UpdateFlagMessage {
        FlagEvent.UpdateFlagMessageEvent updateFlagMsg(FlagEvent.UpdateFlagMessageEvent event);
    }

    @FunctionalInterface
    public interface RemoveFlag {
        void removeFlag(FlagEvent.RemoveFlagEvent event);
    }

    @FunctionalInterface
    public interface AddFlag {
        void addFlag(FlagEvent.AddFlagEvent event);
    }
}

