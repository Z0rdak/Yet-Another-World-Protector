package de.z0rdak.yawp.util.text.messages.multiline;

import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.core.flag.IFlag;

import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;

import java.util.ArrayList;
import java.util.List;

import static de.z0rdak.yawp.api.commands.Commands.buildFlagMsgClearCmd;
import static de.z0rdak.yawp.api.commands.Commands.buildFlagMsgSetCmd;
import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.ChatLinkBuilder.*;
import static de.z0rdak.yawp.util.text.Messages.LINK_COLOR;
import static de.z0rdak.yawp.util.text.Messages.REMOVE_CMD_COLOR;
import static net.minecraft.network.chat.ClickEvent.Action.RUN_COMMAND;
import static net.minecraft.network.chat.ClickEvent.Action.SUGGEST_COMMAND;

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
     *
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
        if (FlagRegister.hasPlayerTag(flag)) {
            MutableComponent alert = buildInfoComponent("cli.flag.msg.mute", "Alert", buildFlagMuteToggleLink(region, flag, false));
            MutableComponent message = buildInfoComponent("cli.flag.msg.text", "Message", buildFlagMessageComponent(region, flag));
            regionInfoLines.add(alert);
            regionInfoLines.add(message);
        }
        return regionInfoLines;
    }

    /**
     * Message: [set] [x]: 'msg' <br>
     */
    public static MutableComponent buildFlagMessageComponent(IProtectedRegion region, IFlag flag) {
        MutableComponent editLink = buildFlagMessageEditLink(region, flag);
        MutableComponent clearLink = buildFlagMessageClearLink(region, flag);
        MutableComponent flagMsgTextWithHover = buildFlagMessageHoverText(region, flag);
        return Messages.substitutable("%s %s '%s'", editLink, clearLink, flagMsgTextWithHover);
    }

    public static MutableComponent buildFlagMessageClearLink(IProtectedRegion region, IFlag flag) {
        MutableComponent hover = Component.translatableWithFallback("cli.flag.msg.text.set.default", "Reset flag message for flag '%s' of '%s' to config default", flag.getName(), region.getName());
        MutableComponent text = Component.translatableWithFallback("cli.link.remove", "x");
        String cmd = buildFlagMsgClearCmd(region, flag.getName());
        return buildExecuteCmdComponent(text, hover, cmd, RUN_COMMAND, REMOVE_CMD_COLOR);
    }

    public static MutableComponent buildFlagMessageEditLink(IProtectedRegion region, IFlag flag) {
        MutableComponent hover = Component.translatableWithFallback("cli.flag.msg.text.set.link.hover", "Change the message shown when the flag '%s' of '%s' is triggered", flag.getName(), region.getName());
        MutableComponent text = Component.translatableWithFallback("cli.flag.msg.text.set.link.text", "Edit");
        String msg = "\"" + flag.getFlagMsg().msg() + "\"";
        String cmd = buildFlagMsgSetCmd(region, flag.getName(), msg);
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
    }

}
