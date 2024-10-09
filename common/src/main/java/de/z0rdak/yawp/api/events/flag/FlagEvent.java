package de.z0rdak.yawp.api.events.flag;

import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.commands.CommandSourceStack;

public abstract class FlagEvent {

    private final IProtectedRegion region;
    private final IFlag flag;
    private final CommandSourceStack src;

    private FlagEvent(final CommandSourceStack src, final IProtectedRegion region, final IFlag flag) {
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

    public static class AddFlagEvent extends FlagEvent {

        public AddFlagEvent(final CommandSourceStack src, final IProtectedRegion region, final IFlag flag) {
            super(src, region, flag);
        }
    }

    public static class RemoveFlagEvent extends FlagEvent {

        public RemoveFlagEvent(final CommandSourceStack src, final IProtectedRegion region, final IFlag flag) {
            super(src, region, flag);
        }
    }

    public static class UpdateFlagMessageEvent extends FlagEvent {

        private String newMsg;

        public UpdateFlagMessageEvent(final CommandSourceStack src, final IProtectedRegion region, final IFlag flag, String newMsg) {
            super(src, region, flag);
            this.newMsg = newMsg;
        }

        public String getNewMsg() {
            return this.newMsg;
        }

        public void setNewMsg(String newMsg) {
            this.newMsg = newMsg;
        }
    }
}
    


