package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.z0rdak.yawp.core.region.GlobalRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;

import java.util.List;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;

public class GlobalCommands {

    private GlobalCommands() {
    }

    public static LiteralArgumentBuilder<CommandSourceStack> build() {
        return literal(GLOBAL)
                .executes(ctx -> CommandUtil.promptRegionInfo(ctx, getGlobalRegion()))
                .then(literal(INFO)
                        .executes(ctx -> CommandUtil.promptRegionInfo(ctx, getGlobalRegion())))
                .then(CommandUtil.buildClearSubCommand((ctx) -> getGlobalRegion()))
                .then(CommandUtil.buildListSubCommand((ctx) -> getGlobalRegion()))
                .then(CommandUtil.buildAddSubCommand((ctx) -> getGlobalRegion()))
                .then(CommandUtil.buildRemoveSubCommand((ctx) -> getGlobalRegion()))
                .then(literal(STATE)
                        .executes(ctx -> promptRegionState(ctx, getGlobalRegion()))
                        .then(literal(ALERT)
                                .executes(ctx -> CommandUtil.setAlertState(ctx, getGlobalRegion(), !getGlobalRegion().isMuted()))
                                .then(Commands.argument(ALERT.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> CommandUtil.setAlertState(ctx, getGlobalRegion(), getAlertArgument(ctx))))
                        )
                        .then(literal(ENABLE)
                                .executes(ctx -> CommandUtil.setActiveState(ctx, getGlobalRegion(), !getGlobalRegion().isActive()))
                                .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> CommandUtil.setActiveState(ctx, getGlobalRegion(), getEnableArgument(ctx))))
                        )
                )
                .then(literal(LIST)
                        .then(literal(DIM)
                                .executes(ctx -> promptDimensionalRegions(ctx, getGlobalRegion(), 0))
                                .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                        .executes(ctx -> promptDimensionalRegions(ctx, getGlobalRegion(), getPageNoArgument(ctx))))
                        )
                )
                .then(literal(RESET).executes(GlobalCommands::resetGlobalRegion));
    }

    private static int promptRegionState(CommandContext<CommandSourceStack> ctx, IProtectedRegion region) {
        return CommandUtil.promptRegionState(ctx, region);
    }

    public static int resetGlobalRegion(CommandContext<CommandSourceStack> ctx) {
        RegionDataManager.get().resetGlobalRegion();
        sendCmdFeedback(ctx.getSource(), new TranslatableComponent("cli.msg.info.region.global.reset", buildRegionInfoLink(getGlobalRegion())));
        return 0;
    }

    private static int promptDimensionalRegions(CommandContext<CommandSourceStack> ctx, GlobalRegion region, int pageNo) {
        List<DimensionRegionCache> dimCaches = RegionDataManager.getDimensionCaches();
        List<MutableComponent> regionPagination = buildPaginationComponents(
                buildRegionListHeader(region),
                buildCommandStr(GLOBAL.toString(), LIST.toString(), DIM.toString()),
                buildResetDimensionalRegionEntries(region, dimCaches),
                pageNo,
                // empty string, since there is now manual creation of dimensional regions
                new TextComponent(""));
        regionPagination.forEach(line -> sendCmdFeedback(ctx.getSource(), line));
        return 0;
    }
}
