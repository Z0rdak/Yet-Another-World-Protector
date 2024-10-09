package de.z0rdak.yawp.util.text.messages.multiline;

import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.ChatLinkBuilder;
import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;

import java.util.ArrayList;
import java.util.List;

import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.api.commands.Commands.*;
import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.text.Messages.*;
import static net.minecraft.ChatFormatting.*;
import static net.minecraft.network.chat.ClickEvent.Action.RUN_COMMAND;
import static net.minecraft.network.chat.ClickEvent.Action.SUGGEST_COMMAND;

public class RegionStateMessage implements MultiLineMessage<IProtectedRegion> {

    private final IProtectedRegion region;
    private final List<Component> regionState;

    public RegionStateMessage(IProtectedRegion region) {
        this.region = region;
        this.regionState = new ArrayList<>();
    }

    public static RegionStateMessage of(IProtectedRegion region) {
        return new RegionStateMessage(region);
    }

    public static MutableComponent buildRegionStateLink(IProtectedRegion region) {
        MutableComponent linkText = Component.translatableWithFallback("cli.msg.info.region.state.link.text", "State");
        MutableComponent hoverText = Component.translatableWithFallback("cli.msg.info.region.state.link.hover", "Show region state for %s", region.getName());
        String cmd = buildRegionStateCmd(region);
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildRegionRenameLink(IMarkableRegion region) {
        String cmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), RENAME.toString(), "");
        MutableComponent text = Component.translatableWithFallback("cli.msg.info.region.state.rename.link.text", "rename");
        MutableComponent hover = Component.translatableWithFallback("cli.msg.info.region.state.rename.link.hover", "Rename region '%s'", region.getName());
        return buildExecuteCmdComponent(text, hover, cmd, SUGGEST_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildRegionPriorityComponent(IMarkableRegion region) {
        int defaultPriorityInc = Services.REGION_CONFIG.getDefaultPriorityInc();
        String incPriorityCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), INC.toString(), String.valueOf(defaultPriorityInc));
        MutableComponent incLinkText = Component.translatableWithFallback("cli.msg.info.region.state.priority.increase.link.text", "+%s", defaultPriorityInc);
        MutableComponent incHoverText = Component.translatableWithFallback("cli.msg.info.region.state.priority.increase.link.hover", "Increase region priority by %s", defaultPriorityInc);
        MutableComponent increaseLink = buildExecuteCmdComponent(incLinkText, incHoverText, incPriorityCmd, RUN_COMMAND, ADD_CMD_COLOR);
        String decPriorityCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), DEC.toString(), String.valueOf(defaultPriorityInc));
        MutableComponent decLinkText = Component.translatableWithFallback("cli.msg.info.region.state.priority.decrease.link.text", "+%s", defaultPriorityInc);
        MutableComponent decHoverText = Component.translatableWithFallback("cli.msg.info.region.state.priority.decrease.link.hover", "Decrease region priority by %s", defaultPriorityInc);
        MutableComponent decreaseLink = buildExecuteCmdComponent(decLinkText, decHoverText, decPriorityCmd, RUN_COMMAND, REMOVE_CMD_COLOR);
        String setPriorityCmd = buildCommandStr(CommandConstants.LOCAL.toString(), region.getDim().location().toString(), region.getName(), STATE.toString(), PRIORITY.toString(), "");
        MutableComponent setPriorityLinkText = Component.translatableWithFallback("cli.msg.info.region.state.priority.set.link.text", "%s", region.getPriority());
        MutableComponent setPriorityHoverText = Component.translatableWithFallback("cli.msg.info.region.state.priority.set.link.hover", "Set priority for region");
        MutableComponent setPriorityLink = buildExecuteCmdComponent(setPriorityLinkText, setPriorityHoverText, setPriorityCmd, SUGGEST_COMMAND, LINK_COLOR);
        return Messages.substitutable("%s %s %s", setPriorityLink, increaseLink, decreaseLink);
    }

    public static MutableComponent buildAllLocalAlertToggleLink(IProtectedRegion dimRegion) {
        MutableComponent enableLinkTextKey = Component.translatableWithFallback("cli.msg.info.region.state.enable.all.true.link.text", "all-on");
        MutableComponent enableHoverTextKey = Component.translatableWithFallback("cli.msg.info.region.state.enable.all.true.link.hover", "Enables alert for all local regions of %s", dimRegion.getName());
        MutableComponent disableLinkTextKey = Component.translatableWithFallback("cli.msg.info.region.state.enable.all.false.link.text", "all-off");
        MutableComponent disableHoverTextKey = Component.translatableWithFallback("cli.msg.info.region.state.enable.all.false.link.hover", "Disables alert for all local regions of %s", dimRegion.getName());
        String enableCmd = buildCommandStr(DIM.toString(), dimRegion.getName(), STATE.toString(), ALERT_LOCAL.toString(), Boolean.TRUE.toString());
        String disableCmd = buildCommandStr(DIM.toString(), dimRegion.getName(), STATE.toString(), ALERT_LOCAL.toString(), Boolean.FALSE.toString());
        MutableComponent activeAlertLink = buildExecuteCmdComponent(enableLinkTextKey, enableHoverTextKey, enableCmd, RUN_COMMAND, GREEN);
        MutableComponent disableAlertLink = buildExecuteCmdComponent(disableLinkTextKey, disableHoverTextKey, disableCmd, RUN_COMMAND, RED);
        return Messages.substitutable("%s %s", activeAlertLink, disableAlertLink);
    }

    public static MutableComponent buildAllLocalEnableComponent(IProtectedRegion dimRegion) {
        MutableComponent enableLinkTextKey = Component.translatableWithFallback("cli.msg.info.region.state.alert.all.true.link.text", "all-on");
        MutableComponent enableHoverTextKey = Component.translatableWithFallback("cli.msg.info.region.state.alert.all.true.link.hover", "Activates all local regions of %s", dimRegion.getName());
        MutableComponent disableLinkTextKey = Component.translatableWithFallback("cli.msg.info.region.state.alert.all.false.link.text", "all-off");
        MutableComponent disableHoverTextKey = Component.translatableWithFallback("cli.msg.info.region.state.alert.all.false.link.hover", "Disables all local regions of %s", dimRegion.getName());
        String enableCmd = buildCommandStr(DIM.toString(), dimRegion.getName(), STATE.toString(), ENABLE_LOCAL.toString(), Boolean.TRUE.toString());
        String disableCmd = buildCommandStr(DIM.toString(), dimRegion.getName(), STATE.toString(), ENABLE_LOCAL.toString(), Boolean.FALSE.toString());
        MutableComponent activeAlertLink = buildExecuteCmdComponent(enableLinkTextKey, enableHoverTextKey, enableCmd, RUN_COMMAND, GREEN);
        MutableComponent disableAlertLink = buildExecuteCmdComponent(disableLinkTextKey, disableHoverTextKey, disableCmd, RUN_COMMAND, RED);
        return Messages.substitutable("%s %s", activeAlertLink, disableAlertLink);
    }

    public static MutableComponent buildRegionEnableComponent(IProtectedRegion region) {
        String linkTextKey = "cli.msg.info.region.state.enable." + region.isActive() + ".link.text";
        String linkFallback = region.isActive() ? "yes" : "no";
        String hoverTextKey = "cli.msg.info.region.state.enable." + !region.isActive() + ".link.hover";
        String hoverFallback = !region.isActive() ? "Enable flag checks" : "Disable flag checks";
        MutableComponent linkText = Component.translatableWithFallback(linkTextKey, linkFallback);
        MutableComponent hoverText = Component.translatableWithFallback(hoverTextKey, hoverFallback);
        ChatFormatting color = region.isActive() ? GREEN : GRAY;
        String cmd = buildRegionStateEnableToggleCmd(region);
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
    }

    public static MutableComponent buildRegionAlertToggleLink(IProtectedRegion region) {
        String linkTextKey = "cli.msg.info.region.state.alert." + !region.isMuted() + ".link.text";
        String hoverTextKey = "cli.msg.info.region.state.alert." + region.isMuted() + ".link.hover";
        String linkFallback = region.isMuted() ? "off" : "on";
        String hoverFallback = !region.isMuted() ? "Turn flag alerts off" : "Turn flag alerts on";
        MutableComponent linkText = Component.translatableWithFallback(linkTextKey, linkFallback);
        MutableComponent hoverText = Component.translatableWithFallback(hoverTextKey, hoverFallback);
        ChatFormatting color = region.isMuted() ? GRAY : GREEN;
        String cmd = buildRegionStateAlertToggleCmd(region);
        return buildExecuteCmdComponent(linkText, hoverText, cmd, RUN_COMMAND, color);
    }

    @Override
    public IProtectedRegion getContent() {
        return this.region;
    }

    /**
     * Builds lines representing the region state
     *
     * @return lines to representing region state
     */
    @Override
    public List<Component> getLines() {
        // == [state] for [<name>] ==
        MutableComponent header = buildHeader(Component.translatableWithFallback("cli.msg.info.header.for", "== %s for %s ==", buildRegionStateLink(this.region), ChatLinkBuilder.buildRegionInfoLink(this.region)));
        this.regionState.add(header);
        MutableComponent regionEnableComponent = buildRegionEnableComponent(this.region);
        MutableComponent regionAlertComponent = buildRegionAlertToggleLink(this.region);
        if (this.region.getRegionType() == RegionType.DIMENSION) {
            // [true|false] | [all-off] [all-on]
            MutableComponent enableComp = Messages.substitutable("%s | %s", regionEnableComponent, buildAllLocalEnableComponent(this.region));
            // [on|off] | [all-off] [all-on]
            MutableComponent alertComp = Messages.substitutable("%s | %s", regionAlertComponent, buildAllLocalAlertToggleLink(this.region));
            this.regionState.add(buildInfoComponent("cli.msg.info.region.state.enable", "Enabled", enableComp));
            this.regionState.add(buildInfoComponent("cli.msg.info.region.state.alert", "Alert", alertComp));
            return this.regionState;
        }
        if (this.region.getRegionType() == RegionType.LOCAL) {
            IMarkableRegion localRegion = (IMarkableRegion) this.region;
            // Name: [<region-name>]
            MutableComponent name = buildInfoComponent("cli.msg.info.region.state.name", "Name", buildRegionRenameLink(localRegion));
            // Priority: <priority> [#][+ 5][- 5]
            MutableComponent priority = buildInfoComponent("cli.msg.info.region.state.priority", "Priority", buildRegionPriorityComponent(localRegion));
            this.regionState.add(name);
            this.regionState.add(priority);
        }
        MutableComponent enableComp = buildInfoComponent("cli.msg.info.region.state.enable", "Enabled", regionEnableComponent);
        MutableComponent alertComp = buildInfoComponent("cli.msg.info.region.state.alert", "Alert", regionAlertComponent);
        // Enabled: [yes], Alert: [on]
        this.regionState.add(Messages.substitutable("%s, %s", enableComp, alertComp));
        return this.regionState;
    }
}
