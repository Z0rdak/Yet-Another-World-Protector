package de.z0rdak.regionshield.commands;

import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.arguments.DimensionArgument;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.World;

import static de.z0rdak.regionshield.commands.CommandUtils.buildLiteralFor;
import static de.z0rdak.regionshield.commands.CommandUtils.getDimensionFromArgument;

public class DimensionCommands {

    private DimensionCommands() {
    }

    public static final LiteralArgumentBuilder<CommandSource> DIMENSION_COMMAND = register();

    public static LiteralArgumentBuilder<CommandSource> register() {
        RequiredArgumentBuilder<CommandSource, ResourceLocation> dimensionArgument = Commands.argument(CommandConstants.DIMENSION.toString(), DimensionArgument.dimension());

        return buildLiteralFor(CommandConstants.DIMENSION)
                .executes(ctx -> promptHelp(ctx.getSource()))
                .then(buildLiteralFor(CommandConstants.HELP)
                        .executes(ctx -> promptHelp(ctx.getSource())))
                .then(buildLiteralFor(CommandConstants.LIST)
                        .executes(ctx -> promptDimensionList(ctx.getSource())))
                .then(buildLiteralFor(CommandConstants.INFO)
                        .executes(ctx -> promptDimensionInfo(ctx.getSource(), ctx.getSource().getLevel().dimension()))
                        .then(dimensionArgument
                                .executes(ctx -> promptDimensionInfo(ctx.getSource(), getDimensionFromArgument(ctx)))));
    }

    private static int promptHelp(CommandSource source) {
        // Prompt help with links ?
        return 0;
    }

    private static int promptDimensionList(CommandSource source) {
        // List prompt with links for info
        return 0;
    }

    private static int promptDimensionInfo(CommandSource source, RegistryKey<World> dim) {
        return 0;
    }

}
