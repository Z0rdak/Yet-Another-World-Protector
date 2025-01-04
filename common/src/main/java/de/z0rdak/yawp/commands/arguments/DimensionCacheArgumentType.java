package de.z0rdak.yawp.commands.arguments;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.data.region.DimensionRegionCache;
import de.z0rdak.yawp.data.region.RegionDataManager;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.core.registries.Registries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.Level;

import java.util.Collection;
import java.util.concurrent.CompletableFuture;

public class DimensionCacheArgumentType implements ArgumentType<DimensionRegionCache> {

    private static final Collection<String> EXAMPLES = RegionDataManager.get().getDimensionList();

    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            dim -> Component.translatableWithFallback("cli.arg.dim.invalid", "Dimension '%s' does not exist!", dim)
    );

    private static DimensionRegionCache getDimensionalRegionCache(ResourceLocation resourcelocation) {
        // TODO: Check valid dimension key?
        ResourceKey<Level> registrykey = ResourceKey.create(Registries.DIMENSION, resourcelocation);
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

    public static DimensionRegionCache getDimRegion(CommandContext<CommandSourceStack> context, String dim) throws CommandSyntaxException {
        ResourceLocation resourcelocation = context.getArgument(dim, ResourceLocation.class);
        boolean isValidDimResourceLocation = context.getSource().levels().stream()
                .map(ResourceKey::location)
                .anyMatch(loc -> loc.equals(resourcelocation));
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
        if (context.getSource() instanceof CommandSourceStack) {
            return SharedSuggestionProvider.suggest(RegionDataManager.get().getDimensionList(), builder);
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
        ResourceLocation resourcelocation = ResourceLocation.read(reader);
        ResourceKey<Level> registrykey = ResourceKey.create(Registries.DIMENSION, resourcelocation);
        if (RegionDataManager.get().containsCacheFor(registrykey)) {
            return RegionDataManager.get().cacheFor(registrykey);
        }
        return null;
    }
}
