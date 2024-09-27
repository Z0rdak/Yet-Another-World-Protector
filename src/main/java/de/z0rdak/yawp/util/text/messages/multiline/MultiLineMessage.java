package de.z0rdak.yawp.util.text.messages.multiline;

import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;

import java.util.List;

public interface MultiLineMessage<T> {

    static <T> void send(CommandSourceStack source, MultiLineMessage<T> message) {
        message.to(source);
    }

    static AreaInfoMessage areaInfo(IMarkableRegion region) {
        return new AreaInfoMessage(region);
    }

    static RegionInfoMessage regionInfo(IProtectedRegion region) {
        return new RegionInfoMessage(region);
    }

    static FlagDetailMessage flagDetail(IProtectedRegion region, IFlag flag) {
        return new FlagDetailMessage(region, flag);
    }

    static RegionStateMessage regionState(IProtectedRegion region) {
        return new RegionStateMessage(region);
    }

    List<Component> getLines();

    T getContent();

    default void to(CommandSourceStack source) {
        this.getLines().forEach(source::sendSystemMessage);
    }
}
