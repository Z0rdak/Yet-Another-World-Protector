package de.z0rdak.regionshield.commands;

import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.z0rdak.regionshield.util.RegionUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.registry.Registry;
import net.minecraft.world.World;

import java.util.Collection;
import java.util.stream.Collectors;

public class CommandUtils {
    public static LiteralArgumentBuilder<CommandSource> buildLiteralFor(CommandConstants constant) {
        return Commands.literal(constant.toString());
    }

    public static RegistryKey<World> getDimensionFromArgument(CommandContext<CommandSource> ctx) {
        ResourceLocation dimResLoc = new ResourceLocation(StringArgumentType.getString(ctx, CommandConstants.DIMENSION.toString()));
        return RegistryKey.create(Registry.DIMENSION_REGISTRY,dimResLoc);
    }

    public static Collection<String> getQuotedDimensionList() {
        return RegionUtil.getDimensionList().stream()
                .map(dim -> "'" + dim + "'")
                .collect(Collectors.toList());
    }
}
