package de.z0rdak.yawp.api.events.flag;

import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.commands.CommandSourceStack;

public abstract class FlagEvent {

    private final IMarkableRegion region;
    private final IFlag flag;
    private final CommandSourceStack src;

    private FlagEvent(CommandSourceStack src, IMarkableRegion region, IFlag flag) {
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

    public IMarkableRegion getRegion() {
        return region;
    }
    
    public class AddFlagEvent extends FlagEvent {

        private AddFlagEvent(CommandSourceStack src, IMarkableRegion region, IFlag flag) {
            super(src, region, flag);
        }
    }

    public class RemoveFlagEvent extends FlagEvent {

        private RemoveFlagEvent(CommandSourceStack src, IMarkableRegion region, IFlag flag) {
            super(src, region, flag);
        }
    }
    
    public class UpdateFlagMessageEvent extends FlagEvent {

        private String currentMsg;
        private String newMsg;
        
        private UpdateFlagMessageEvent(CommandSourceStack src, IMarkableRegion region, IFlag flag) {
            super(src, region, flag);
        }
    }
}
    


