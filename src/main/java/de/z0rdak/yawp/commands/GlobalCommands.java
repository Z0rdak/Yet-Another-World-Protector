package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.z0rdak.yawp.core.region.GlobalRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.StringTextComponent;

import java.util.List;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;

public class GlobalCommands {

    private GlobalCommands() {
    }

    public static LiteralArgumentBuilder<CommandSource> build() {
        return literal(GLOBAL)
                .executes(ctx -> CommandUtil.promptRegionInfo(ctx, getGlobalRegion(), RegionType.GLOBAL))
                .then(literal(INFO)
                        .executes(ctx -> CommandUtil.promptRegionInfo(ctx, getGlobalRegion(), RegionType.GLOBAL)))
                .then(CommandUtil.buildClearSubCommand((ctx) -> getGlobalRegion(), RegionType.GLOBAL))
                .then(CommandUtil.buildListSubCommand((ctx) -> getGlobalRegion(), RegionType.GLOBAL))
                .then(CommandUtil.buildAddSubCommand((ctx) -> getGlobalRegion(), RegionType.GLOBAL))
                .then(CommandUtil.buildRemoveSubCommand((ctx) -> getGlobalRegion(), RegionType.GLOBAL))
                .then(literal(STATE)
                        .executes(ctx -> promptRegionState(ctx, getGlobalRegion()))
                        .then(literal(ALERT)
                                .executes(ctx -> CommandUtil.setAlertState(ctx, getGlobalRegion(), RegionType.GLOBAL, !getGlobalRegion().isMuted()))
                                .then(Commands.argument(ALERT.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> CommandUtil.setAlertState(ctx, getGlobalRegion(), RegionType.GLOBAL, getAlertArgument(ctx))))
                        )
                        .then(literal(ENABLE)
                                .executes(ctx -> CommandUtil.setActiveState(ctx, getGlobalRegion(), RegionType.GLOBAL, !getGlobalRegion().isActive()))
                                .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> CommandUtil.setActiveState(ctx, getGlobalRegion(), RegionType.GLOBAL, getEnableArgument(ctx))))
                        )
                )
                .then(literal(LIST)
                        .then(literal(DIM)
                                .executes(ctx -> promptDimensionalRegions(ctx, getGlobalRegion(), 0))
                                .then(Commands.argument(PAGE.toString(), IntegerArgumentType.integer(0))
                                        .executes(ctx -> promptDimensionalRegions(ctx, getGlobalRegion(), getPageNoArgument(ctx))))
                        )
                );
    }

    private static int setActiveState(CommandContext<CommandSource> ctx, IProtectedRegion region, boolean activate) {
        return CommandUtil.setActiveState(ctx, region, RegionType.GLOBAL, activate);
    }

    private static int setActiveState(CommandContext<CommandSource> ctx, IProtectedRegion region) {
        return setActiveState(ctx, region, !region.isActive());
    }

    private static int setAlertState(CommandContext<CommandSource> ctx, IProtectedRegion region, boolean showAlert) {
        return CommandUtil.setAlertState(ctx, region, RegionType.GLOBAL, showAlert);
    }

    private static int setAlertState(CommandContext<CommandSource> ctx, IProtectedRegion region) {
        return setAlertState(ctx, region, !region.isMuted());
    }

    private static int promptRegionState(CommandContext<CommandSource> ctx, IProtectedRegion region) {
        CommandUtil.promptRegionState(ctx, region, RegionType.GLOBAL);
        return 0;
    }

    private static int promptDimensionalRegions(CommandContext<CommandSource> ctx, GlobalRegion region, int pageNo) {
        List<DimensionRegionCache> dimCaches = RegionDataManager.getDimensionCaches();
        List<IFormattableTextComponent> regionPagination = buildPaginationComponents(
                buildRegionListHeader(region, RegionType.GLOBAL),
                buildCommandStr(GLOBAL.toString(), LIST.toString(), DIM.toString()),
                buildResetDimensionalRegionEntries(region, dimCaches, RegionType.GLOBAL),
                pageNo,
                // empty string, since there is now manual creation of dimensional regions
                new StringTextComponent(""));
        regionPagination.forEach(line -> sendCmdFeedback(ctx.getSource(), line));
        return 0;
    }
}
