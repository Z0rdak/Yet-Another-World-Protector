package de.z0rdak.yawp.commands.arguments;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.command.CommandSource;
import net.minecraft.registry.RegistryKey;
import net.minecraft.registry.RegistryKeys;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.text.Text;
import net.minecraft.util.Identifier;
import net.minecraft.world.World;

import java.util.Collection;
import java.util.concurrent.CompletableFuture;

public class DimensionCacheArgumentType implements ArgumentType<DimensionRegionCache> {

    private static final Collection<String> EXAMPLES = RegionDataManager.get().getDimensionList();

    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            dim -> Text.translatable("cli.arg.dim.invalid", dim)
    );

    private static DimensionRegionCache getDimensionalRegionCache(Identifier resourcelocation) {
        // TODO: Check valid dimension key?
        RegistryKey<World> registrykey = RegistryKey.of(RegistryKeys.WORLD, resourcelocation);
        if (RegionDataManager.get().containsCacheFor(registrykey)) {
            return RegionDataManager.get().cacheFor(registrykey);
        } else {
            DimensionRegionCache newCache = RegionDataManager.get().newCacheFor(registrykey);
            RegionDataManager.save();
            return newCache;
        }
    }

    public static DimensionCacheArgumentType dimRegion() {
        return new DimensionCacheArgumentType();
    }

    public static DimensionRegionCache getDimRegion(CommandContext<ServerCommandSource> context, String dim) throws CommandSyntaxException {
        Identifier resourcelocation = context.getArgument(dim, Identifier.class);
        boolean isValidDimResourceLocation = context.getSource().getWorldKeys().stream().map(RegistryKey::getValue).anyMatch(loc -> loc.equals(resourcelocation));
        if (isValidDimResourceLocation) {
            DimensionRegionCache dimCache = getDimensionalRegionCache(resourcelocation);
            if (dimCache == null) {
                throw ERROR_INVALID_VALUE.create(resourcelocation.toString());
            }
            return dimCache;
        } else {
            throw ERROR_INVALID_VALUE.create(resourcelocation.toString());
        }
    }

    @Override
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof ServerCommandSource) {
            return CommandSource.suggestMatching(RegionDataManager.get().getDimensionList(), builder);
        } else {
            return Suggestions.empty();
        }
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }

    @Override
    public DimensionRegionCache parse(StringReader reader) throws CommandSyntaxException {
        Identifier resourcelocation = Identifier.fromCommandInput(reader);
        RegistryKey<World> registrykey = RegistryKey.of(RegistryKeys.WORLD, resourcelocation);
        if (RegionDataManager.get().containsCacheFor(registrykey)) {
            return RegionDataManager.get().cacheFor(registrykey);
        }
        return null;
    }
}
