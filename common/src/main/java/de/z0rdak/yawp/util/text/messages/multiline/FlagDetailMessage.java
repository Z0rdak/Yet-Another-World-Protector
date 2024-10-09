package de.z0rdak.yawp.util.text.messages.multiline;

import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;

import java.util.ArrayList;
import java.util.List;

import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildFlagMuteToggleLink;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildFlagOverrideToggleLink;

public class FlagDetailMessage implements MultiLineMessage<IFlag> {

    private final IProtectedRegion region;
    private final IFlag flag;
    private final List<Component> regionInfoLines;

    public FlagDetailMessage(IProtectedRegion region, IFlag flag) {
        this.region = region;
        this.flag = flag;
        this.regionInfoLines = new ArrayList<>();
    }

    @Override
    public IFlag getContent() {
        return this.flag;
    }

    /**
     * Builds the flag info component for the given flag and region. <br>
     * == Flag info for [flagname] of [region] == <br>
     * State: state [set state] <br>
     * Alert: [active] <br>
     * Override: [false] <br>
     * Message: [set] [x]: 'msg' <br>
     */
    @Override
    public List<Component> getLines() {
        regionInfoLines.clear();
        MutableComponent header = buildFlagInfoHeader(region, flag);
        regionInfoLines.add(header);
        MutableComponent state = buildInfoComponent("cli.flag.state", "State", buildFlagStateComponent(region, flag));
        MutableComponent override = buildInfoComponent("cli.flag.override", "Override", buildFlagOverrideToggleLink(region, flag, false));
        regionInfoLines.add(state);
        regionInfoLines.add(override);
        if (RegionFlag.hasPlayerCategory(flag)) {
            MutableComponent alert = buildInfoComponent("cli.flag.msg.mute", "Alert", buildFlagMuteToggleLink(region, flag, false));
            MutableComponent message = buildInfoComponent("cli.flag.msg.text", "Message", buildFlagMessageComponent(region, flag));
            regionInfoLines.add(alert);
            regionInfoLines.add(message);
        }
        return regionInfoLines;
    }

}
