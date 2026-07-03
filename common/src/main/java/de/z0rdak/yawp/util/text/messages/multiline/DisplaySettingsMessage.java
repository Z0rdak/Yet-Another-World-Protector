package de.z0rdak.yawp.util.text.messages.multiline;

import de.z0rdak.yawp.core.area.visuals.BlockDisplayProperties;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.util.text.Messages;
import net.minecraft.core.Holder;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.world.level.block.Block;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static de.z0rdak.yawp.api.commands.Commands.*;
import static de.z0rdak.yawp.util.ChatComponentBuilder.*;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildInfoComponent;
import static de.z0rdak.yawp.util.ChatLinkBuilder.*;
import static de.z0rdak.yawp.util.text.Messages.LINK_COLOR;
import static net.minecraft.ChatFormatting.*;
import static net.minecraft.ChatFormatting.RESET;
import static net.minecraft.network.chat.ClickEvent.Action.RUN_COMMAND;
import static net.minecraft.network.chat.ClickEvent.Action.SUGGEST_COMMAND;

public class DisplaySettingsMessage implements MultiLineMessage<BlockDisplayProperties> {

    private final IMarkableRegion region;
    private final List<Component> infoLines;

    public DisplaySettingsMessage(IMarkableRegion region) {
        this.region = region;
        this.infoLines = new ArrayList<>();
    }

    /**
     *  == Display Settings for [region]  == <br>
     * Block: [minecraft:cyan_stained_glass] | [set] <br>
     * Glow: enabled | [on] [off] <br>
     * Light-Level: 15 | [set] <br>
     */
    @Override
    public List<Component> getLines() {
        infoLines.clear();
        var header = buildHeader(Component.translatableWithFallback("cli.msg.info.header.of", "== %s of %s ==", buildDisplaySettingsLink(region), buildRegionInfoLink(region)));
        var blockSubject = Component.translatableWithFallback("cli.msg.info.region.display.block", "Block");
        var glowSubject = Component.translatableWithFallback("cli.msg.info.region.display.glow", "Glow");
        var lightLevelSubject = Component.translatableWithFallback("cli.msg.info.region.display.light-level", "Light-Level");

        var blockComponent = buildInfoComponent(blockSubject, buildDisplayBlockInfo(region), buildSetDisplayBlockLink(region));
        var glowActions = Messages.substitutable("%s %s", buildSetDisplayGlowOnLink(region), buildSetDisplayGlowOffLink(region));
        var glowComponent = buildInfoComponent(glowSubject, buildDisplayGlowInfo(region), glowActions);
        var lightLevelComponent = buildInfoComponent(lightLevelSubject, buildDisplayLightLevelInfo(region), buildSetDisplayLightLevelLink(region));

        infoLines.add(header);
        infoLines.add(blockComponent);
        infoLines.add(glowComponent);
        infoLines.add(lightLevelComponent);
        return infoLines;
    }

    @Override
    public BlockDisplayProperties getContent() {
        return region.getArea().getDisplay();
    }

    /**
     *  [Cyan Stained Glass] { Hover: minecraft:cyan_stained_glass }
     */
    public static MutableComponent buildDisplayBlockInfo(IMarkableRegion region) {
        var blockInfoHover = Component.literal(region.getArea().getDisplay().blockRl().toString());
        Optional<Holder.Reference<Block>> block = BuiltInRegistries.BLOCK.get(region.getArea().getDisplay().blockRl());
        if (block.isPresent()) {
            var blockInfoText = block.get().value().getName();
            return buildTextWithWhiteBracketsAndHover(blockInfoText, blockInfoHover, BLUE);
        }
        throw new IllegalStateException("BlockRl '" + region.getArea().getDisplay().blockRl().toString() + "' in region " + region.getName() + " is invalid.");
    }

    public static MutableComponent buildSetDisplayBlockLink(IMarkableRegion region) {
        var text = Component.translatableWithFallback("cli.msg.info.region.display.block.link.text", "set");
        var hover = Component.translatableWithFallback("cli.msg.info.region.display.block.link.hover", "Set display block for area of %s", region.getName());
        return buildExecuteCmdLinkWithBrackets(text, hover, buildSetDisplayBlockCommand(region), SUGGEST_COMMAND, LINK_COLOR);
    }

    /**
     * enabled { Hover: Glow effect for area display is [] }
     */
    public static MutableComponent buildDisplayGlowInfo(IMarkableRegion region) {
        boolean hasGlow = region.getArea().getDisplay().hasGlow();
        var text = hasGlow
                ? Component.translatableWithFallback("cli.msg.info.region.display.glow-on.text", "enabled")
                : Component.translatableWithFallback("cli.msg.info.region.display.glow-off.text", "disabled");
        var hover = Component.translatableWithFallback("cli.msg.info.region.display.glow.hover",
                "Glow effect for area display is %s", hasGlow ? "enabled" : "disabled");
        return buildTextWithHoverMsg(text, hover, hasGlow ? GREEN : RED);
    }

    public static MutableComponent buildDisplayLightLevelInfo(IMarkableRegion region) {
        var text = Component.literal(String.valueOf(region.getArea().getDisplay().lightLevel()));
        var hover = Component.translatableWithFallback("cli.msg.info.region.display.light-level.hover",
                "Light level for area display is %s", region.getArea().getDisplay().lightLevel());
        return buildTextWithHoverMsg(text, hover, RESET);
    }

    public static MutableComponent buildSetDisplayGlowOnLink(IMarkableRegion region) {
        var text = Component.translatableWithFallback("cli.msg.info.region.display.glow-on.link.text", "on");
        var hover = Component.translatableWithFallback("cli.msg.info.region.display.glow-on.link.hover", "Enable glow effect for block display");
        return buildExecuteCmdLinkWithBrackets(text, hover, buildSetDisplayGlowCommand(region, true), RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildSetDisplayGlowOffLink(IMarkableRegion region) {
        var text = Component.translatableWithFallback("cli.msg.info.region.display.glow-off.link.text", "off");
        var hover = Component.translatableWithFallback("cli.msg.info.region.display.glow-off.link.hover", "Disable glow effect for block display");
        return buildExecuteCmdLinkWithBrackets(text, hover, buildSetDisplayGlowCommand(region, false), RUN_COMMAND, LINK_COLOR);
    }

    public static MutableComponent buildSetDisplayLightLevelLink(IMarkableRegion region) {
        var text = Component.translatableWithFallback("cli.msg.info.region.display.light-level.link.text", "set");
        var hover = Component.translatableWithFallback("cli.msg.info.region.display.light-level.link.hover", "Set light level for area display");
        return buildExecuteCmdLinkWithBrackets(text, hover, buildSetDisplayLightLevelCommand(region), SUGGEST_COMMAND, LINK_COLOR);
    }
}
