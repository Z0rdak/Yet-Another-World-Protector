package de.z0rdak.yawp.api.events.flag;

import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.commands.CommandSourceStack;
import net.minecraftforge.eventbus.api.Event;

public abstract class ForgeFlagEvent extends Event {

    private final IProtectedRegion region;
    private final IFlag flag;
    private final CommandSourceStack src;

    private ForgeFlagEvent(final CommandSourceStack src, final IProtectedRegion region, final IFlag flag) {
        this.src = src;
        this.region = region;
        this.flag = flag;
    }

    public CommandSourceStack getSrc() {
        return src;
    }

    public IFlag getFlag() {
        return flag;
    }

    public IProtectedRegion getRegion() {
        return region;
    }

    public static class AddFlagEvent extends ForgeFlagEvent {

        public AddFlagEvent(final CommandSourceStack src, final IProtectedRegion region, final IFlag flag) {
            super(src, region, flag);
        }

        public AddFlagEvent(FlagEvent.AddFlagEvent event) {
            super(event.getSrc(), event.getRegion(), event.getFlag());
        }
    }

    public static class RemoveFlagEvent extends ForgeFlagEvent {

        public RemoveFlagEvent(final CommandSourceStack src, final IProtectedRegion region, final IFlag flag) {
            super(src, region, flag);
        }

        public RemoveFlagEvent(FlagEvent.RemoveFlagEvent event) {
            super(event.getSrc(), event.getRegion(), event.getFlag());
        }
    }

    public static class UpdateFlagMessageEvent extends ForgeFlagEvent {

        private String newMsg;

        public UpdateFlagMessageEvent(final CommandSourceStack src, final IProtectedRegion region, final IFlag flag, String newMsg) {
            super(src, region, flag);
            this.newMsg = newMsg;
        }

        public UpdateFlagMessageEvent(FlagEvent.UpdateFlagMessageEvent event) {
            this(event.getSrc(), event.getRegion(), event.getFlag(), event.getNewMsg());
        }

        public static FlagEvent.UpdateFlagMessageEvent asNonEvent(UpdateFlagMessageEvent forgeEvent) {
            return new FlagEvent.UpdateFlagMessageEvent(forgeEvent.getSrc(), forgeEvent.getRegion(), forgeEvent.getFlag(), forgeEvent.getNewMsg());
        }

        public static UpdateFlagMessageEvent asEvent(FlagEvent.UpdateFlagMessageEvent event) {
            return new UpdateFlagMessageEvent(event.getSrc(), event.getRegion(), event.getFlag(), event.getNewMsg());
        }

        public String getNewMsg() {
            return this.newMsg;
        }

        public void setNewMsg(String newMsg) {
            this.newMsg = newMsg;
        }
    }
}
    


